module type S = sig
  type t = float

  type z = Base.u = A | B

  type a = int

  val x : int

  module M : sig
    type 'a t = 'a option

    val x : int
  end

  module type S = sig
    type t
  end

  module F (X : S) : sig
    type t = X.t
  end

  module N : sig
    type t = int
  end

  module O : sig
    type t = int

    type u = float

    type 'a v = 'a option
  end

  module P : sig
    type t = Base.P.t = A of z

    type u = bool
  end
end
