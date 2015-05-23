#!/usr/bin/env ocamlscript
let open Ocamlscript.Std in
begin
  Ocaml.packs := ["ppx_deriving";"ppx_deriving.show"];
end
--
()
type t = int * string [@@deriving show]
