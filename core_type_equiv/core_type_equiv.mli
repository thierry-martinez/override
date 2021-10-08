val equal_loc :
    ('a -> 'a -> bool) -> 'a Location.loc -> 'a Location.loc -> bool

val equiv_core_type :
    (Ppxlib.core_type -> Ppxlib.core_type -> bool) ->
      Ppxlib.core_type -> Ppxlib.core_type -> bool
