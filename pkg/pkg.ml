#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ltree" @@ fun c->
  Ok [ Pkg.lib "pkg/META"
     ; Pkg.mllib ~api:["Ltree"] "ltree/ltree.mllib" ]
