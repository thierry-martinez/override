val mkloc : 'a -> 'a Location.loc

val var_of_type_expr : Types.type_expr -> string option

val univar_of_type_expr : Types.type_expr -> string option

val core_type_of_type_expr : Types.type_expr -> Ppxlib.core_type

val type_declaration :
    string -> Types.type_declaration -> Ppxlib.type_declaration

val signature : Types.signature -> Ppxlib.signature

val module_type : Types.module_type -> Ppxlib.module_type
