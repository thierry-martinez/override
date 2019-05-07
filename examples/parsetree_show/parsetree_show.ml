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
  [%%types] [@@deriving show]
end
