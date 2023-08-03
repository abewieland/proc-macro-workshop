use quote::{quote, format_ident};
use syn::{
    spanned::Spanned, parse_macro_input, DeriveInput, Data, Fields, Ident, Type,
    PathArguments, GenericArgument, Expr, Lit, Meta, Error
};

enum FieldType {
    // Normal fields have a single initializer
    Normal,
    // Option fields are automatically detected via declaration (Option<T>)
    Option,
    // Vector fields must be a vector and specify a single item acceptor
    Vector(Ident),
}

use FieldType::*;

struct Field<'a> {
    // The field name
    name: &'a Ident,
    // The scalar type T
    t: &'a Type,
    // The field type, as specified above
    ft: FieldType,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder = format_ident!("{}Builder", name);

    let fields = match get_fields(&input.data) {
        Ok(fields) => fields,
        Err(err) => return err.to_compile_error().into()
    };

    let field_names: Vec<&Ident> = fields.iter().map(|f| {
        f.name
    }).collect();
    let rfield_names = &field_names;

    let field_defs = fields.iter().map(|f| {
        let name = f.name;
        let t = f.t;
        match f.ft {
            Normal | Option => quote! { #name: std::option::Option<#t> },
            Vector(_) => quote! { #name: std::vec::Vec<#t> }
        }
    });

    let field_inits = fields.iter().map(|f| {
        let name = f.name;
        match f.ft {
            Normal | Option => quote! { #name: std::option::Option::None },
            Vector(_) => quote! { #name: std::vec::Vec::new() }
        }
    });

    let init_funcs = fields.iter().map(|f| {
        let name = f.name;
        let t = f.t;
        match &f.ft {
            Normal | Option => quote! {
                pub fn #name(&mut self, #name: #t) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            Vector(func) => {
                let mut first = quote! {};
                if name != &func.to_string() {
                    first = quote! {
                        pub fn #name(&mut self, #name: &mut std::vec::Vec<#t>) -> &mut Self {
                            self.#name.append(#name);
                            self
                        }
                    }
                }
                quote! {
                    #first
                    pub fn #func(&mut self, #name: #t) -> &mut Self {
                        self.#name.push(#name);
                        self
                    }
                }
            }
        }
    });

    let build_vars = fields.iter().map(|f| {
        let name = f.name;
        match f.ft {
            Normal => {
                let msg = format!("{} not initialized", name);
                quote! {
                    let #name = self.#name.as_ref().ok_or(#msg)?;
                }
            }
            Option | Vector(_) => quote! {
                let #name = &self.#name;
            }
        }
    });

    let result = quote! {
        pub struct #builder {
            #(#field_defs,)*
        }

        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#field_inits,)*
                }
            }
        }

        impl #builder {
            #(#init_funcs)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #(#build_vars)*
                std::result::Result::Ok(#name {
                    #(#rfield_names: #rfield_names.clone(),)*
                })
            }
        }
    };

    proc_macro::TokenStream::from(result)
}

fn parse_field(f: &syn::Field) -> Result<Field, Error> {
    let mut names: Vec<Ident> = Vec::new();
    for attr in &f.attrs {
        if let Meta::List(ref list) = attr.meta {
            if list.path.segments.len() == 1 && list.path.segments.first().unwrap().ident == "builder" {
                if let Ok(Expr::Assign(expr)) = syn::parse::<Expr>(list.tokens.clone().into()) {
                    if let Expr::Path(ref ep) = *expr.left {
                        if ep.path.segments.len() == 1 && ep.path.segments.first().unwrap().ident == "each" {
                            if let Expr::Lit(ref el) = *expr.right {
                                if let Lit::Str(ref ls) = el.lit {
                                    names.push(Ident::new(&ls.value(), ls.span()));
                                    continue;
                                }
                            }
                        }
                    }
                }
                return Err(Error::new_spanned(list, "expected `builder(each = \"...\")`"));
            }
        }
    }
    if names.len() > 1 {
        return Err(Error::new_spanned(f, "too many builder attributes"));
    }
    if names.len() == 1 {
        if let Type::Path(ref path) = f.ty {
            if let None = path.qself {
                if path.path.segments.len() == 1 {
                    let seg = path.path.segments.first().unwrap();
                    if seg.ident == "Vec" {
                        if let PathArguments::AngleBracketed(ref args) = seg.arguments {
                            if args.args.len() == 1 {
                                if let GenericArgument::Type(ref ty) = args.args.first().unwrap() {
                                    return Ok(Field {
                                        name: f.ident.as_ref().unwrap(),
                                        t: ty,
                                        ft: Vector(names.pop().unwrap()),
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        return Err(Error::new(f.ty.span(), "expected `Vec<...>`"));
    }
    if let Type::Path(ref path) = f.ty {
        if let None = path.qself {
            if path.path.segments.len() == 1 {
                let seg = path.path.segments.first().unwrap();
                if seg.ident == "Option" {
                    if let PathArguments::AngleBracketed(ref args) = seg.arguments {
                        if args.args.len() == 1 {
                            if let GenericArgument::Type(ref ty) = args.args.first().unwrap() {
                                return Ok(Field {
                                    name: f.ident.as_ref().unwrap(),
                                    t: ty,
                                    ft: Option,
                                });
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(Field {
        name: f.ident.as_ref().unwrap(),
        t: &f.ty,
        ft: Normal,
    })
}

fn get_fields(data: &Data) -> Result<Vec<Field>, Error> {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => fields.named.iter().map(parse_field).collect(),
                Fields::Unnamed(ref f) => Err(Error::new(f.span(), "expected named fields")),
                Fields::Unit => Err(Error::new(data.struct_token.span, "expected fields"))
            }
        },
        Data::Enum(ref e) => Err(Error::new(e.enum_token.span, "expected struct")),
        Data::Union(ref u) => Err(Error::new(u.union_token.span, "expected struct"))
    }
}
