(lang dune 2.9)
(name graphqlcli)
(package
 (name graphqlcli)
 (synopsis "CLI for interacting with GraphQL and making queries")
 (description "CLI for graphql")
 (depends ocaml dune ppx_deriving ppx_expect
          ppx_sexp_conv sexplib)
 (tags
  (topics "to describe" your project)))
