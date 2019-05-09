module%override Stdlib : sig
  module%override Lexing : sig
    [%%types] [@@deriving show]
  end
end

module%override Longident : sig
  [%%types] [@@deriving show]
end

module%override Location : sig
  [%%types] [@@deriving show]
end

module%override Asttypes : sig
  [%%types] [@@deriving show]
end

module%override Parsetree : sig
  type toplevel_phrase and co [@@remove]

  [%%types] [@@deriving show]

  type toplevel_phrase = _ and co
end
