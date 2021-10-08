module%override Astlib__ = struct
module%override Longident = struct
  type t = _ [@@rewrite] [@@deriving refl]
end
end

module%import Astlib__ = struct
module%override Location = struct
  type t = Astlib__.Location.t [@opaque] [@@from: Astlib__.Location.t] [@@rewrite] [@@deriving refl]

  type 'a loc = _ [@@deriving refl]
end

module%override Ast_412 = struct
module%override Asttypes = struct
  [%%recursive [%%types]] [@@deriving refl]
end

module%override Parsetree = struct
  [%%recursive [%%types]] [@@deriving refl]
end
end
end

let equal_loc equal_txt (l1 : 'a Location.loc) (l2 : 'a Location.loc) =
  equal_txt l1.txt l2.txt

let equiv_core_type (equiv : Ppxlib.core_type -> Ppxlib.core_type -> bool)
    t0 t1 =
  let sub_hook : type a b . (a, b) Refl.Eq.hook_fun =
  fun refl0 refl1 super x0 x1 ->
    match refl0, refl1 with
    | Ast_412.Parsetree.Refl_core_type, Ast_412.Parsetree.Refl_core_type ->
        equiv x0 x1
    | _ ->
        super x0 x1 in
  let hook : type a b . (a, b) Refl.Eq.hook_fun =
  fun _refl0 _refl1 super x0 x1 ->
    super ~hook:{ hook = sub_hook } x0 x1 in
  Refl.equal ~hook:{ hook } [%refl: Ast_412.Parsetree.core_type] [] t0 t1
