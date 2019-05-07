type t = int

type u = A | B

let x = 1

module M = struct
  type 'a t = 'a option

  let x = 2
end

module type S = sig
  type t
end

module F (X : S) = struct
  type t
end

module N = struct
  type t
end

module O = struct
  type t = int

  type u = bool

  type 'a v = 'a option
end

module P = struct
  type t = A of u

  type u = bool

  type 'a v = 'a option
end
