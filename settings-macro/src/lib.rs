use darling::{FromDeriveInput, FromField, FromMeta, ToTokens};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Path};

extern crate proc_macro;

#[derive(Debug, FromDeriveInput)]
#[darling(supports(struct_named))]
struct SettingsReceiver {
  ident: syn::Ident,
  data: darling::ast::Data<(), SettingReceiver>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GetterType {
  Deref,
  Ref,
  Copy,
}

impl Default for GetterType {
  fn default() -> Self {
    Self::Copy
  }
}

impl FromMeta for GetterType {
  fn from_string(value: &str) -> darling::Result<Self> {
    match value {
      "deref" => Ok(Self::Deref),
      "ref" => Ok(Self::Ref),
      x => Err(darling::Error::custom(format!(
        "unknown getter type '{}'",
        x
      ))),
    }
  }
}

#[derive(Debug, FromField)]
#[darling(attributes(setting), forward_attrs(doc))]
struct SettingReceiver {
  ident: Option<syn::Ident>,
  ty: syn::Type,
  attrs: Vec<syn::Attribute>,
  #[darling(default)]
  get: GetterType,
  #[darling(default)]
  hidden: bool,
  #[darling(default)]
  default_fn: Option<Path>,
  #[darling(default)]
  default: Option<syn::Lit>,
}

impl SettingReceiver {
  fn getter(&self) -> TokenStream {
    if self.hidden {
      return Default::default();
    }
    let ty = &self.ty;
    let ident = self.ident.as_ref().unwrap();
    let real_type = match self.get {
      GetterType::Copy => ty.to_token_stream(),
      GetterType::Deref => quote! { &<#ty as ::core::ops::Deref>::Target },
      GetterType::Ref => quote! { &#ty },
    };
    let and = match self.get {
      GetterType::Copy => None,
      _ => Some(quote! {&}),
    };
    let doc_attrs = self.attrs.iter().filter(|a| a.path.is_ident("doc"));
    quote! {
      #(#doc_attrs)*
      pub fn #ident(&self) -> #real_type {
        #and self.#ident
      }
    }
  }

  fn def(&self) -> syn::Result<TokenStream> {
    let id = self.ident.as_ref().unwrap();
    let ty = &self.ty;
    Ok(if let Some(p) = self.default.as_ref() {
      quote! { #id: #ty::from(#p) }
    } else if let Some(p) = self.default_fn.as_ref() {
      quote! { #id: #p () }
    } else {
      quote! { #id: Default::default() }
    })
  }
}

#[proc_macro_derive(Settings, attributes(setting))]
pub fn settings(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let item = parse_macro_input!(_item as syn::DeriveInput);
  let SettingsReceiver { ident, data } = SettingsReceiver::from_derive_input(&item).unwrap();

  let fields = data.take_struct().unwrap().fields;

  let getters = fields.iter().map(|s| s.getter());
  let defaults = fields
    .iter()
    .map(|s| s.def())
    .collect::<syn::Result<Vec<_>>>()
    .unwrap();

  quote! {
    impl #ident {
      #(#getters)*
    }

    impl Default for #ident {
      fn default() -> Self {
        Self {
          #(#defaults),*
        }
      }
    }
  }
  .into()
}
