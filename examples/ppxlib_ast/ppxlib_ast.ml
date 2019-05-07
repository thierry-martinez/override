(* This example implements mechanically the transformations rule
   described in the comments of
   https://github.com/ocaml-ppx/ppxlib/blob/master/ast/ast.ml *)

[%%recursive
  module%import Stdlib = struct
    module%import Lexing = struct
      type position = _ [@@rewrite]
    end
  end

  module%import Location = struct
    type location = _ [@@from: t] [@@rewrite]

    type 'a loc = _ [@@rewrite]
  end

  module%import Longident = struct
    type longident = _ [@@from: t] [@@rewrite]
  end

  type longident_loc = longident loc [@@rewrite]

  module%import Asttypes = struct
    type constant [@@remove]

    type 'a loc [@@rewrite] [@@remove]

    [%%types] [@@rewrite]
  end

  module%import Parsetree = struct
    [%%types]
  end]
