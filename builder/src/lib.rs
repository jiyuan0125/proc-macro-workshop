use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Expr, ExprLit, Field, Fields, GenericArgument, Ident, Lit, Type};

struct StructMeta<'a> {
    struct_ident: &'a Ident,
    fields: Vec<FieldMeta<'a>>,
}

struct FieldMeta<'a> {
    ident: &'a Ident,
    ty: &'a Type,
    is_option: bool,
    each: std::option::Option<EachMeta<'a>>,
}

struct EachMeta<'a> {
    ident: Ident,
    ty: &'a Type,
}

impl<'a> TryFrom<&'a DeriveInput> for StructMeta<'a> {
    type Error = syn::Error;

    fn try_from(value: &'a DeriveInput) -> std::result::Result<Self, Self::Error> {
        let struct_ident = &value.ident;
        let Data::Struct(data_struct) = &value.data else {
            return Err(syn::Error::new_spanned(struct_ident, "expected struct"));
        };
        let Fields::Named(fields) = &data_struct.fields else {
            return Err(syn::Error::new_spanned(
                struct_ident,
                "expected named fields",
            ));
        };
        Ok(Self {
            struct_ident,
            fields: fields
                .named
                .iter()
                .map(FieldMeta::try_from)
                .collect::<std::result::Result<_, _>>()?,
        })
    }
}

impl<'a> TryFrom<&'a Field> for FieldMeta<'a> {
    type Error = syn::Error;

    fn try_from(field: &'a Field) -> std::result::Result<Self, Self::Error> {
        let ident = field.ident.as_ref().unwrap();
        let inner_ty = extract_inner_type(&field.ty, "Option");
        let is_option = inner_ty.is_some();
        let ty = match inner_ty {
            Some(inner_ty) => inner_ty,
            None => &field.ty,
        };

        let attr_builder = field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("builder"));

        let mut each_ident = None;
        if let Some(attr_builder) = attr_builder {
            let meta = attr_builder
                .parse_args::<syn::MetaNameValue>()
                .map_err(|_| {
                    syn::Error::new_spanned(
                        &attr_builder.meta,
                        "expected `builder(each = \"...\")`",
                    )
                })?;

            if !meta.path.is_ident("each") {
                return Err(syn::Error::new_spanned(
                    &attr_builder.meta,
                    "expected `builder(each = \"...\")`",
                ));
            }

            each_ident = match meta.value {
                Expr::Lit(ExprLit {
                    lit: Lit::Str(lit_str),
                    ..
                }) => lit_str.parse::<Ident>().ok(),
                _ => None,
            }
            .filter(|each_ident| *each_ident != *ident);
        }

        match each_ident {
            Some(each_ident) => {
                let inner_ty = extract_inner_type(ty, "Vec")
                    .ok_or(syn::Error::new_spanned(ty, "expected Vec"))?;

                Ok(Self {
                    ident,
                    ty,
                    is_option,
                    each: Some(EachMeta {
                        ident: each_ident,
                        ty: inner_ty,
                    }),
                })
            }
            None => Ok(Self {
                ident,
                ty,
                is_option,
                each: None,
            }),
        }
    }
}

impl StructMeta<'_> {
    fn gen_code(&self) -> std::result::Result<proc_macro2::TokenStream, syn::Error> {
        let struct_ident = &self.struct_ident;
        let builder_ident = Ident::new(&format!("{}Builder", struct_ident), struct_ident.span());

        let builder_fields = self.fields.iter().map(|field| {
            let field_ident = &field.ident;
            let field_ty = &field.ty;
            quote::quote! {
                #field_ident: std::option::Option<#field_ty>
            }
        });

        let builder_field_init_values = self.fields.iter().map(|field| {
            let field_ident = &field.ident;
            quote::quote! {
                #field_ident: None
            }
        });

        let builder_set_methods = self.fields.iter().map(|field| {
            let field_ident = &field.ident;
            let field_ty = &field.ty;

            let mut token_stream = quote::quote! {
                pub fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                    self.#field_ident = Some(#field_ident);
                    self
                }
            };

            if let Some(each) = &field.each {
                let each_ident = &each.ident;
                let each_ty = &each.ty;
                token_stream.extend(quote::quote! {
                    pub fn #each_ident(&mut self, #each_ident: #each_ty) -> &mut Self {
                        self.#field_ident.get_or_insert_with(Vec::new).push(#each_ident);
                        self
                    }
                });
            }

            token_stream
        });

        let builder_build_fields = self.fields.iter().map(|field| {
            let field_ident = &field.ident;

            let field_value = match field.is_option {
                true => quote::quote! { self.#field_ident.clone() },
                false => quote::quote! { self.#field_ident.clone().unwrap_or_default() },
            };

            quote::quote! {
                #field_ident: #field_value
            }
        });

        Ok(quote::quote! {
            pub struct #builder_ident {
                #(#builder_fields,)*
            }

            impl #builder_ident {
                #(#builder_set_methods)*

                pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                    Ok(#struct_ident {
                        #(#builder_build_fields,)*
                    })
                }
            }

            impl #struct_ident {
                pub fn builder() -> #builder_ident {
                    #builder_ident {
                        #(#builder_field_init_values,)*
                    }
                }
            }
        })
    }
}

fn extract_inner_type<'a>(field_ty: &'a Type, arg: &str) -> Option<&'a Type> {
    let Type::Path(type_path) = field_ty else {
        return None;
    };

    let Some(path_segment) = type_path.path.segments.iter().last() else {
        return None;
    };

    if path_segment.ident != arg {
        return None;
    }

    let syn::PathArguments::AngleBracketed(angle_bracketed) = &path_segment.arguments else {
        return None;
    };

    let Some(generic_argument) = angle_bracketed.args.iter().next() else {
        return None;
    };

    let GenericArgument::Type(inner_ty) = generic_argument else {
        return None;
    };

    Some(inner_ty)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    StructMeta::try_from(&input)
        .and_then(|meta| meta.gen_code())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
