open Migrate_parsetree.OCaml_407.Ast

val equiv_core_type :
    (Parsetree.core_type -> Parsetree.core_type -> bool) ->
      Parsetree.core_type -> Parsetree.core_type -> bool
