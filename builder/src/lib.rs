use core::panic;

use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Lit,
    Meta, MetaNameValue, Path, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match safe_derive(input) {
        Ok(res) => res,
        Err(err) => err.into_compile_error().into(),
    }
}

fn safe_derive(input: DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let builder_struct_fields = builder_fields_iter(&input.data, BuilderIterType::StructType)?;
    let builder_init_fields = builder_fields_iter(&input.data, BuilderIterType::Initialization)?;
    let setters = builder_fields_iter(&input.data, BuilderIterType::SetterDeclaration)?;
    let build_aggregation = builder_fields_iter(&input.data, BuilderIterType::BuildAggregation)?;
    let expanded = quote! {
        use std::error::Error;
        pub struct #builder_name {
            #builder_struct_fields
        }
        impl #struct_name {
            fn builder() -> #builder_name {
                #builder_name {
                    #builder_init_fields
                }
            }
        }
        impl #builder_name {
            #setters

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn Error>> {
                Ok(#struct_name {
                    #build_aggregation
                })
            }
        }
    };
    Ok(TokenStream::from(expanded))
}

// determine what to do when iterating fields
enum BuilderIterType {
    StructType,
    Initialization,
    SetterDeclaration,
    BuildAggregation,
}

fn builder_fields_iter(
    data: &Data,
    iter_type: BuilderIterType,
) -> syn::Result<proc_macro2::TokenStream> {
    if let Data::Struct(DataStruct {
        fields: syn::Fields::Named(ref fields),
        ..
    }) = data
    {
        // Expands to an expression like
        //
        //         executable: std::option::Option<String>,
        //         args: std::option::Option<Vec<String>>,
        //         env: std::option::Option<Vec<String>>,
        //         current_dir: std::option::Option<String>,
        //
        // or
        //
        //         executable: std::option::Option::None,
        //         args: std::option::Option::None,
        //         env: std::option::Option::None,
        //         current_dir: std::option::Option::None,
        //
        let mut mapped_fields = vec![];
        for f in fields.named.iter() {
            let name = &f.ident;
            let ty = &f.ty;
            match iter_type {
                BuilderIterType::Initialization => {
                    let line = if let std::option::Option::Some(_) =
                        capture_single_inner_type(ty, "Option".into())
                    {
                        // init type std::option::Option<std::option::Option<T>> with std::option::Option::Some(std::option::Option::None)
                        quote_spanned!(f.span() =>#name: std::option::Option::Some(std::option::Option::None),)
                    } else if let std::option::Option::Some(_) =
                        capture_single_inner_type(ty, "Vec".into())
                    {
                        quote_spanned!(f.span() =>#name: std::option::Option::Some(vec![]),)
                    } else {
                        quote_spanned!(f.span() =>#name: std::option::Option::None,)
                    };
                    mapped_fields.push(line);
                }
                BuilderIterType::StructType => {
                    mapped_fields.push(quote_spanned!(f.span()=>#name: std::option::Option<#ty>,))
                }
                BuilderIterType::SetterDeclaration => {
                    if let std::option::Option::Some(captured_type) =
                        capture_single_inner_type(ty, "Option".into())
                    {
                        let capture_vec_each = capture_vec_each_method_name(f)?;
                        let ele_setter = if let std::option::Option::Some(ident) = capture_vec_each
                        {
                            let element_type =
                                capture_single_inner_type(captured_type, "Vec".into())
                                    .expect("extract vec fails");
                            quote_spanned!(f.span() =>
                                pub fn #ident(&mut self, #ident: #element_type) -> &mut Self {
                                    if let std::option::Option::Some(std::option::Option::Some(ref mut arr)) = self.#name {
                                        arr.push(#ident);
                                    } else {
                                        self.#name = std::option::Option::Some(std::option::Option::Some(vec![#ident]));
                                    }
                                    self
                                }
                            )
                        } else {
                            let full_setter = quote_spanned!(f.span() =>
                                pub fn #name(&mut self, #name: #captured_type) -> &mut Self {
                                    self.#name = std::option::Option::Some(std::option::Option::Some(#name));
                                    self
                                }
                            );
                            full_setter
                        };
                        mapped_fields.push(quote_spanned!(f.span() => #ele_setter))
                    } else {
                        // not wrapped in Option
                        let capture_vec_each = capture_vec_each_method_name(f)?;
                        let ele_setter = if let std::option::Option::Some(ident) = capture_vec_each
                        {
                            let element_type = capture_single_inner_type(ty, "Vec".into())
                                .expect("extract vec fails");
                            quote_spanned!(f.span() =>
                                pub fn #ident(&mut self, #ident: #element_type) -> &mut Self {
                                    if let std::option::Option::Some(ref mut arr) = self.#name {
                                        arr.push(#ident);
                                    } else {
                                        self.#name = std::option::Option::Some(vec![#ident]);
                                    }
                                    self
                                }
                            )
                        } else {
                            let full_setter = quote_spanned!(f.span() =>
                                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = std::option::Option::Some(#name);
                                    self
                                }
                            );
                            full_setter
                        };
                        mapped_fields.push(quote_spanned!(f.span() => #ele_setter))
                    }
                }
                BuilderIterType::BuildAggregation => mapped_fields.push(quote_spanned!(f.span() =>
                    #name: self.#name.take().ok_or(format!("{} unset", stringify!(#name)))?,
                )),
            }
        }
        let recurse = mapped_fields.iter();
        Ok(quote! {
            #(#recurse)*
        })
    } else {
        panic!("Invalid input")
    }
}

fn capture_single_inner_type(ty: &Type, ident: String) -> std::option::Option<&syn::Type> {
    // the structure of type is described in tests/06
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if let std::option::Option::Some(seg) = segments.first() {
            if seg.ident == ident {
                assert!(segments.len() == 1, "<T> should have only one segment");
                // capture T in std::option::Option<T>
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    assert!(args.len() == 1, "<T> should have only one segment");
                    if let std::option::Option::Some(syn::GenericArgument::Type(inner_type)) =
                        args.first()
                    {
                        return std::option::Option::Some(inner_type);
                    }
                }
            }
        }
    }
    std::option::Option::None
}

// capture `arg` in `each = "arg"`
fn capture_vec_each_method_name(field: &Field) -> syn::Result<std::option::Option<syn::Ident>> {
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            if let Ok(Meta::NameValue(MetaNameValue { path, value, .. })) = attr.parse_args() {
                if path.is_ident("each") {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(ref val_str),
                        ..
                    }) = value
                    {
                        return Ok(std::option::Option::Some(syn::Ident::new(
                            val_str.value().as_str(),
                            attr.span(),
                        )));
                    }
                }
                let err = format!("expected `builder(each = \"...\")`");
                return Err(syn::Error::new_spanned(attr.meta.clone(), err));
            }
        }
    }
    Ok(std::option::Option::None)
}
