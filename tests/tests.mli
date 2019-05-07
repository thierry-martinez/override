[%%override: module Base : sig
  type t = float

  type z = _ [@@from: u] [@@rewrite]

  type a = int

  [%%override: module M : sig
    type 'a t = _ [@@deriving]
  end]

  [%%override: module F (X : S) : sig
    type t = X.t
  end]

  module N : sig
    type t = int
  end

  [%%override: module O : sig
    type u = float

    [%%types] [@@deriving]
  end]

  [%%override: module P : sig
    [%%recursive:
      type v [@@remove]
      [%%types] [@@deriving]]
  end]

  [%%override: module G (Y : S) : sig
    [%%types]
  end]
end]
