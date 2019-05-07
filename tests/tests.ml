[%%override module Base : Tests_spec.S = struct
  type t = float

  type z = _ [@@from: u] [@@rewrite]

  type a = int

  [%%override module M = struct
    type 'a t = _ [@@deriving]
  end]

  [%%override module F (X : S) = struct
    type t = X.t
  end]

  module N = struct
    type t = int
  end

  [%%override module O = struct
    type u = float

    [%%types] [@@deriving]
  end]

  [%%override module P = struct
    [%%recursive
      type v [@@remove]
      [%%types] [@@deriving]]
  end]
end]
