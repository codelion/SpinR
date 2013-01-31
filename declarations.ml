(*_ $Id: declarations.ml 4527 2012-10-17 13:08:20Z weissmam $

Copyright (c) 2010 - 2012 Technische Universitaet Muenchen, TUM
Copyright (c) 2010 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
3. Neither the name of TUM nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)

module M = Map.Make(Identifier)

type value = (Type.t * Expression.t option)
type t = value M.t
type key = M.key

exception Type_mismatch of (key * Type.t * Type.t)
exception Initial_value_mismatch of (key * Expression.t * Expression.t)

let empty = M.empty

let find = M.find

let mem = M.mem

let remove = M.remove

let fold = M.fold

let iter = M.iter

let add id (t', def') m =
  try
    let (t, def) = find id m in
    match (t, t', def, def') with
    | (t, t', _, _) when t <> t' -> raise (Type_mismatch (id, t, t'))
    | (_, _, Some v, Some v') when v <> v' -> raise (Initial_value_mismatch (id, v, v'))
    | _ -> M.add id (t, def) m
  with Not_found ->
    M.add id (t', def') m

let string_of_decl id =
  function
  | (t, Some ex) ->
    Printf.sprintf "%s %s = %s" (Type.string_of t) (Identifier.string_of id) (Expression.string_of ex)
  | (t, None) ->
    Printf.sprintf "%s %s" (Type.string_of t) (Identifier.string_of id)

let string_of m =
  fold (fun id value str -> str ^ (string_of_decl id value) ^ "\n") m ""

let to_channel oc m =
  iter (fun id value -> Printf.fprintf oc "%s\n" (string_of_decl id value)) m

let map f m =
  fold (fun id value m' -> let (id', value') = f id value in add id' value' m') m empty

let rename f =
  map (fun id value -> (f id, value))

let union a b =
  fold (fun id value m -> add id value m) b a

let filter p m =
  fold (fun id value m' -> if p id value then add id value m' else m') m empty

