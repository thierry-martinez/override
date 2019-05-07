[%%recursive
  [%%import module Stdlib = struct
    [%%import module Lexing = struct
      type position = _ [@@rewrite]
    end]
  end]

  [%%import module Location = struct
    type location = _ [@@from: t] [@@rewrite]

    type 'a loc = _ [@@rewrite]
  end]

  [%%import module Longident = struct
    type longident = _ [@@from: t] [@@rewrite]
  end]

  type longident_loc = longident loc [@@rewrite]

  [%%import module Asttypes = struct
    type constant [@@remove]

    type 'a loc [@@rewrite] [@@remove]

    [%%types] [@@rewrite]
  end]

  [%%import module Parsetree = struct
    [%%types]
  end]]
