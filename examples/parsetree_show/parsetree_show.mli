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
  [%%types] [@@deriving show]
end