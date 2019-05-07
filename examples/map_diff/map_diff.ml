module%override Map = struct
  module%override Make (X : OrderedType) = struct
    let diff m0 m1 =
      merge (fun key v0 v1 -> match v1 with None -> v0 | Some _ -> None) m0 m1
  end
end
