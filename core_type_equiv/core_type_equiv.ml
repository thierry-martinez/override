module%override Ast_407 = struct
  module%override Location = struct
    let equal _ _ = true

    type 'a loc = _ [@@deriving eq]
  end

  module%override Longident = struct
    [%%types] [@@deriving eq]
  end

  module%override Asttypes = struct
    type 'a loc [@@rewrite] [@@remove]
  
    type constant [@@remove]
  
    [%%types] [@@deriving eq]
  end
    
  module type S = sig
    val equiv_core_type :
        Parsetree.core_type -> Parsetree.core_type -> bool
  end
  
  module Make (X: S) = struct
    module%include Parsetree = struct
      type core_type = Parsetree.core_type [@equal X.equiv_core_type]
            [@@rewrite] [@@remove]
  
      type toplevel_phrase and co [@@remove]
  
      [%%types] [@@deriving eq]
  
      type core_type = _ [@@deriving eq]
    end
  end
end

let equiv_core_type equiv t0 t1 =
  let module Equiv = Ast_407.Make (struct let equiv_core_type = equiv end) in
  Equiv.equal_core_type t0 t1
