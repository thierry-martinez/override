module%override Base : Tests_spec.S = struct
  type t = float

  type z = _ [@@from: u] [@@rewrite]

  type a = int

  module%override M = struct
    type 'a t = _ [@@deriving]
  end

  module%override F (X : S) = struct
    type t = X.t
  end

  module N = struct
    type t = int
  end

  module%override O = struct
    type u = float

    [%%types] [@@deriving]
  end

  module%override P = struct
    [%%recursive
      type v [@@remove]
      [%%types] [@@deriving]]
  end

  module%override G (Y : S) = struct
    [%%types]
  end
end
