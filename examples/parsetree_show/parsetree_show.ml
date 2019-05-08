module%override Stdlib = struct
  module%override Lexing = struct
    [%%types] [@@deriving show]
  end
end

module%override Longident = struct
  [%%types] [@@deriving show]
end

module%override Location = struct
  [%%types] [@@deriving show]
end

module%override Asttypes = struct
  [%%types] [@@deriving show]
end

module%override Parsetree = struct
  type toplevel_phrase and co [@@remove]

  [%%types] [@@deriving show]

  type toplevel_phrase = _ and co
end
