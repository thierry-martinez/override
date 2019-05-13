# OCaml PPX extension for overriding modules

This library extends OCaml syntax for overriding modules defined in
other compiled interface files.  This library generalizes
[`ppx_import`] by allowing a whole module to be imported with all its
types, possibly with annotations.  In particular, importing a whole
module can be convenient to apply [`ppx_deriving`] to a large family
of mutually inductive data types (see examples below).

[`ppx_import`]: https://github.com/ocaml-ppx/ppx_import
[`ppx_deriving`]: https://github.com/ocaml-ppx/ppx_deriving

## Usage

Similarly to `ppx_import`, you may require the `override` package in
the `staged_pps` field of your `dune` file.

```ocaml
(library
  (name foo)
  (preprocess (staged_pps override ppx_deriving.show)))
```

You may use `override` in the toplevel with the help of
[`findlib`].

[`findlib`]: http://projects.camlcity.org/projects/findlib.html

```ocaml
#use "topfind";;
#require "override";;
```

## Overriding submodules

In its simplest form, this syntax extension can be seen as a
mechanization of Gabriel Scherer's post about [Overriding submodules]
on Gagallium blog
(it is worth noticing that, thanks to [GPR#1892], module overriding is
now free with OCaml 4.08, so readers interested about what this library can
still offer with new versions of OCaml may skip this section).

[Overriding submodules]: http://gallium.inria.fr/blog/overriding-submodules/
[GPR#1892]: https://github.com/ocaml/ocaml/pull/1892

Let's begin with the same example as in the blog post, with the
following module defined in [`toto.ml`].

[`toto.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/gagallium/toto.ml

```ocaml
let x = 1
module Tata = struct
  let y = 2
end
module Titi = struct
  let z = 3
end
```

In another file, say [`gagallium.ml`], we can override the module
`Toto` to change the definition of `y` for a `string`, for example.

[`gagallium.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/gagallium/gagallium.ml

```ocaml
module%override Toto = struct
  module%override Tata = struct
    let y = "2"
  end
end
```

This works for interfaces too: we can write the following interface in
[`gagallium.mli`], in the same lines as above.

[`gagallium.mli`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/gagallium/gagallium.mli

```ocaml
module%override Toto : sig
  module%override Tata : sig
    val y : string
  end
end
```

This works with functors too: we can add for example a function `diff`
to modules generated by `Map.Make` ([`map_diff.ml`]).

[`map_diff.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/map_diff/map_diff.ml

```ocaml
module%override Map = struct
  module%override Make (X : OrderedType) = struct
    let diff m0 m1 =
      merge (fun key v0 v1 -> match v1 with None -> v0 | Some _ -> None) m0 m1
  end
end
```

This works with module types too: the following example changes the
definition of [Hashtbl.HashedType] to take a [compare] predicate instead
of [equal] ([`hashed_type_compare.ml`]).
Note that since there is no `with module type` constraint, the signature
of `Hashtbl` have to be expanded in the code generated by the notation
to remove the original `HashedType` definition.

[`hashed_type_compare.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/hashed_type_compare/hashed_type_compare.ml

```ocaml
module%override Hashtbl = struct
  module type HashedType = sig
    type t

    val compare : t -> t -> int

    val hash : t -> int
  end

  module Make (X : HashedType) = struct
    include Hashtbl.Make (struct
      type t = X.t

      let equal x y =
        X.compare x y = 0

      let hash = X.hash
    end)
  end
end
```

## Importing types

If the imported module defines a type, we can override its definition
with the construction `type t = _`: this construction allows new attributes to
be added to the type.
For instance, the following example (adapted from `ppx_import`) derives
`Longident.show` from the definition of `Longident.t`
([`longident_show.ml`]).

[`longident_show.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/longident_show/longident_show.ml

```ocaml
module%override Longident = struct
  type t = _  [@@deriving show]
end

let () =
  print_endline (Longident.show (Longident.parse "Foo.Bar.baz"))
(* Longident.Ldot (Longident.Ldot (Longident.Lident ("Foo"), "Bar"), "baz") *)
```

Several types may be imported at once with the construction
`type t and ... and u = _`.
All the types imported at once are redefined in a single mutually recursive
definition.

```ocaml
module%override Location = struct
  type t and 'a loc = _ [@@deriving show]
end
```

## `and co`: importing mutually recursive types

Ending a `type` list with `and co` include the other types that are
defined in the same mutually recursive definitions as one of the types
explicitly listed.
Each added type carries the attributes given after `and co`.

The `and co` notation will be recognized if it ends the enumeration:
if you want to refer to an abstract type called `co`, you may refer to
it at another place in the enumeration.

```ocaml
module%overrive Example = struct
  type a and b = _ and co [@@deriving show]
end
```

## `[@@from: ... ]`: renaming types

Names of the types can be changed: the type declaration introduces the
new type and the imported name is carried by the `[@@from: ...]`
notation.

```ocaml
module%override Location = struct
  type location = _ [@@from: t]
end
```

## `[%%types]`: importing all the types and module types from a module

The notation `[%%types]` imports all the types and module types
defined in the currently overriden module, except types and moduel
types that have been overriden previously. Attributes given to
`[%%types]` are applied to each imported type. Types that were
mutually recursive in the imported module are defined as mutually
recursive in the overriden module (to gather independent types in a
single mutually recursive group, see the next section about the
`[%%recursive]` extension).

For example, we may try to derive `show` for all types of OCaml `Parsetree`,
using the package `compiler-libs.common`.

```ocaml
module%override Parsetree = struct
  [%%types] [@@deriving show]
end
```

We get the following error.

```ocaml
File "_none_", line 1:
Error: Unbound value Asttypes.pp_loc
```

We can fix the error by deriving `show` for `Asttypes` as well.

```ocaml
module%override Asttypes = struct
  [%%types] [@@deriving show]
end

module%override Parsetree = struct
  [%%types] [@@deriving show]
end
```

We will get errors for `Location.pp` and `Longident.pp` that we will
fix in the same way. More interestingly, with OCaml 4.07 and above, we
will get the following error.

```ocaml
File "_none_", line 1:
Error: Unbound value Stdlib.Lexing.pp_position
```

This error can be solved by overriding both the module `Stdlib` and
the submodule `Lexing`. All in all, `show` can be derived for `Parsetree`
and all its dependencies as below ([`parsetree_show.ml`]).
(This example supposes that OCaml 4.07 or above is used: for former versions
of OCaml, `Lexing` should just be imported as the other modules.)

[`parsetree_show.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/parsetree_show/parsetree_show.ml

```ocaml
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
```

In the case of `Asttypes` and `Parsetree`, the imported interfaces
only contain types, and all these types are redefined (with
`[%%types]`). When these two conditions are met, the modules
themselves are not included. Since `Asttypes` and `Parsetree`
interfaces have no implementations, there would have been a link-time
failure if they were included.

## `[%%symbols]`: importing all the symbols from a module

The notation `[%%symbols]` can be used in signatures to import all
the symbols (types, module types, values and module declarations) that
are defined in the imported module. The following example defines
the signature of a module for ordered and hashed types by constructing
the union of `Hashtbl.HashedType` and `Hashtbl.OrderedType`
([`ordered_hashed_type.ml`]).

[`ordered_hashed_type.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/ordered_hashed_type/ordered_hashed_type.ml

```ocaml
module type S = sig
  module%import Hashtbl : sig
    module type%import HashedType = sig
      [%%symbols]
    end
  end

  module%import Map : sig
    module type%import OrderedType = sig
      type t [@@remove]
      [%%symbols]
    end
  end
end
```

## `[@@remove]`: removing types

Types may be removed from signature with the annotation `[@@remove]`.
Several types may be removed at once by listing them with `and`,
and the `and co` notation may be used as well.

```ocaml
module%override Asttypes = struct
  type constant [@@remove]

  [%%types] [@@deriving show]
end
```

Note that to exclude some types from the annotations carried by
`[%%types]` without removing them from the module, the types just
have to be imported before invoking `[%%types]`.

```ocaml
module%override Asttypes = struct
  type constant = _

  [%%types] [@@deriving show]
end
```

If the excluded types have to be imported after `[%%types]` (for
instance, if the excluded types refer to the other types), it is
possible to remove them first and reimport them afterwards.

```ocaml
module%override Parsetree = struct
  type toplevel_phrase and co [@@remove]

  [%%types] [@@deriving show]

  type toplevel_phrase = _ and co
end
```

## `module%include`, `module%import`, `[%%recursive]`: flattening structure

The notation `module%include` overrides a module and include it in the
current module.
In the following example, the types `location` and `'a loc` are then
defined at top-level (and not in a module `Location`).

```ocaml
module%include Location = struct
  type location [@@from: t] and 'a loc = _ [@@deriving show]
end
```

The notation `module%import` overrides a module in the current module
without including the definitions that are not explicitely overriden.
In the following example, the type `loc` is defined at top-level as an
alias for `Location.t`, but the type `'a loc` is not imported.

```ocaml
module%import Location = struct
  type loc = _ [@@from: t] [@@deriving show]
end
```

The notation `recursive` transforms a set of type definitions into a
single mutually recursive definition of types. The annotations put to
`recursive` are applied to one of the types (the first one). This can
be useful for some derivers. The `recursive` notation will reject
structures that contain other items than type definitions, but
`module%import` can be used inside `recursive` as long as the
structure contains only type definitions. In the following example,
`location`, `'a loc` and `longident` are defined in a single mutually
recursive type definition.

```ocaml
[%%recursive
  module%import Location = struct
    type location = _ [@@from: t]

    type 'a loc = _
  end

  module%import Longident = struct
    type longident = _ [@@from: t]
  end]
```

Note that the notation becomes `[%%recursive: ...]` in a signature.

## Self import of types declared in the interface file

The notation `module%import` is useful in particular to import the types
declared in the interface file. For example, the implementation file
[`self_import.ml`] can import the types declared in [`self_import.mli`]
with the following construction.

[`self_import.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/self_import/self_import.ml
[`self_import.mli`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/self_import/self_import.mli

```ocaml
module%import Self_import = struct
  [%%types]
end
```

This construction can be used at the root of the implementation file
as well as in submodules, including functors.
For instance, the following interface file [`self_import.mli`] declares
a type, a module type and a functor, and some values.

```ocaml
type t = A | B

val x : t

module type S = sig
  type t

  val perform : t -> unit
end

module Make (X : S) : sig
  type t = X.t

  val perform_twice : t -> unit
end
```

The following implementation file [`self_import.ml`] imports these
definitions and provides the declared values.

```ocaml
module%import Self_import = struct
  [%%types]
end

let x = A

module Make (X : S) = struct
  module%import Self_import = struct
    module%import Make (X : S) = struct
      [%%types]
    end
  end

  let perform_twice t =
    X.perform t;
    X.perform t
end
```

## `[@@rewrite]`: rewriting types

Type declarations can be annotated with `[@@rewrite]` attribute to use
them as rewriting rules for subsequent type importations. Let's consider
the following example.

```ocaml
module%override Location = struct
  type location = _ [@@from: t]

  type 'a loc = _
end
```

The type `Location.t` is renamed into `Location.location` but
the imported definition of `loc` still refers to `Location.t`, which can
be indesirable if that definition is used for deriving.
By putting the annotation `[@@rewrite]` to the definition of `location`,
the rewriting rule `t` → `location` is applied to every type importation
that appears after the definition of `location` in the module.

```ocaml
module%override Location = struct
  type location = _ [@@from: t] [@@rewrite]
  (* Rewriting rules at this point: t -> location *)

  type 'a loc = _
  (* Will be defined as type 'a loc = { txt : 'a; loc : location },
     where the type of loc has been rewritten. *)
end
(* Rewriting rules at this point: Location.t -> Location.location *)
```

The scope of rewriting rules extends to the end of the outer-most
block that carries an override notation (`module%override`, `recursive`, ...).
The notation `[%%rewrite ...]` can be used to introduce a rewriting scope
explicitely (scopes can be nested, and rewriting rules in outer scopes apply
in inner scopes). Rewriting rules are promoted by qualifying the identifiers
accordingly when their scope extends after the module where they are defined.
For instance, the rule `t` → `location` defined in the example above would
become `Location.t` → `Location.location` if applied outside the module
`Location`.

The notation `[@@rewrite]` can be applied to a type alias as well.  In
the following example, `Longident.t loc` will be rewritten `longident
loc` by the rewriting rule introduced by the first type declaration,
and then `longident_loc` by the second (rewriting rules are applied up
to reaching a fix point).

```ocaml
[%%types
  module%import Longident = struct
    type longident = _ [@@from: t] [@@rewrite]
    (* Rewriting rules at this point: t -> longident *)
  end
  (* Rewriting rules at this point: Longident.t -> longident *)

  type longident_loc = longident Location.loc [@@rewrite]
  (* Rewriting rules at this point:
     - Longident.t -> longident
     - longident Location.loc -> longident_loc

     The following rewriting rule is induced by composition:
     - Longident.t Location.loc -> longident_loc *)
]
```

When applied to a removed type, the rewriting rule is oriented in the
opposite direction. For instance, the following example transforms references
to `'a Asttypes.loc` into `'a Location.loc`.

```ocaml
module%override Asttypes = struct
  type 'a loc [@@rewrite] [@@remove]
  (* Rewriting rules at this point: 'a loc -> 'a Location.loc *)
end
(* Rewriting rules at this point: a Asttypes.loc -> 'a Location.loc *)
```

If the removed type is declared as a type alias, the right-hand side of the
declaration is used as the right-hand side of the rewriting rule.
This can be useful for instance to annotate the occurrences of some types .

For instance, the following example (adapted from [`ppx_import`]) derives
`pp_package_type` from the definition of `Parsetree.package_type`
([`package_type.ml`]), relying on rewriting rules to add annotations
to the occurrences of `Longident.t` and `Asttypes.loc`.

[`package_type.ml`]: https://gitlab.inria.fr/tmartine/override/blob/master/examples/package_type/package_type.ml

```ocaml
[%%rewrite
  module%import Longident = struct
    type t = Longident.t [@printer Printtyp.longident]
          [@@rewrite] [@@remove]
    (* Rewriting rules at this point:
       - t -> Longident.t [@printer Printtyp.longident] *)
  end
  (* Rewriting rules at this point:
     - Longident.t -> Longident.t [@printer Printtyp.longident] *)

  module%import Asttypes = struct
    type 'a loc = 'a Asttypes.loc
          [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt]
          [@@rewrite] [@@remove]
  end

  module%import Parsetree = struct
    type core_type = Parsetree.core_type [@printer Pprintast.core_type]
          [@@rewrite] [@@remove]

    (* Rewriting rules at this point:
       - Longident.t -> Longident.t [@printer Printtyp.longident]
       - 'a Asttypes.loc -> 'a Asttypes.loc
          [@polyprinter fun pp fmt x -> pp fmt x.Asttypes.txt]
       - core_type -> Parsetree.core_type [@printer Pprintast.core_type] *)

    type package_type = _ [@@deriving show]
  end]
```

The following example implements mechanically the transformation rules
described in the comments of [`ast/ast.ml`] from `ppxlib`.

[`ast/ast.ml`]: https://github.com/ocaml-ppx/ppxlib/blob/master/ast/ast.ml.

```ocaml
(* "- replacing app [type ...] by [and ...] to make everything one
recursive block" *)
[%%recursive

  (* "- adding the type definitions for position, location, loc and longident"
     They are imported from Stdlib.Lexing.position, Location.t, 'a Location.loc,
     and Longident.t respectively. *)

  (* "- flattening all the modules"
     All the modules are imported with module%import. *)
  module%import Stdlib = struct
    module%import Lexing = struct
      type position = _ [@@rewrite]
    end
  end

  module%import Location = struct
    (* "- renaming a few types:
        - - Location.t -> location" *)
    type location = _ [@@from: t] [@@rewrite]

    type 'a loc = _ [@@rewrite]
  end

  module%import Longident = struc
    (* "- renaming a few types:
        - - Longident.t -> longident" *)t
    type longident = _ [@@from: t] [@@rewrite]
  end

  (* "- adding a type longident_loc = longident loc and replacing all
   the occurences of the latter by the former. This is so that we can
   override iteration an the level of a longident loc." *)
  type longident_loc = longident loc [@@rewrite]

  module%import Asttypes = struct
    (* "- removing Asttypes.constant (unused and conflicts with
       Parsetree.constant)" *)
    type constant [@@remove]

    type 'a loc [@@rewrite] [@@remove]

    [%%types] [@@rewrite]
  end

  module%import Parsetree = struct
    [%%types]
  end]
```