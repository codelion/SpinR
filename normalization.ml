(*_ $Id: normalization.ml 4527 2012-10-17 13:08:20Z weissmam $

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

module List = struct (*$< List *)
  include List
  let exactly_one p xs =
    match List.filter p xs with
    | [x] -> true
    | _ -> false

  (*$T exactly_one
    exactly_one (fun x -> x = 1) [0;1;2;3;4;5;6] = true
    exactly_one (fun x -> x = 1) [0;1;2;3;1;5;6] = false
    exactly_one (fun x -> x = 1) [0;0;2;3;4;5;6] = false
  *)

  let all_but_one p =
    exactly_one (fun elt -> not (p elt))

  (*$T all_but_one
    all_but_one (fun x -> x = 1) [1;1;1;1;1;0;1;1;1] = true
    all_but_one (fun x -> x = 1) [1;1;1;1;1;1;1;1;1] = false
    all_but_one (fun x -> x = 1) [1;1;1;1;1;1;1;1;9] = true
    all_but_one (fun x -> x = 1) [1;8;1;1;1;1;1;1;1] = true
  *)

end (*$>*)

(** add an `Atomic block around all primitive statements *)
let add_atomic =
  let rec atomice stmt =
    match stmt with
    | `Atomic [x] -> stmt
    | `Dstep [x] -> `Atomic [x]
    | `Atomic xs -> `Atomic (List.map atomice xs)
    | `Dstep xs -> `Atomic (List.map atomice xs)
    | `If xss -> `If (List.map (List.map atomice) xss)
    | `IfElse (xss, ys) -> `IfElse (List.map (List.map atomice) xss, List.map atomice ys)
    | `Assign _
    | `Goto _
    | `Label _
    | `Print _
    | `Assert _
    | `Guard _
    | `Run _
    | `Skip
    | `Comment _ -> `Atomic [stmt]
  in
  List.map atomice

(*$T add_atomic
  add_atomic [`Skip] = [`Atomic [`Skip]]
  add_atomic [`Atomic [`Skip]]  = [`Atomic [`Skip]]
  add_atomic [`Skip; `Skip]  = [`Atomic [`Skip]; `Atomic [`Skip]]
  add_atomic [`Atomic [`Skip; `Skip]]  = [`Atomic [`Atomic [`Skip]; `Atomic [`Skip]]]
*)


(** fuse consecutive `Atomic blocks as long as the resulting `Atomic block only contains
    at most one statement that has influence on the model *)
let fuse_atomic body =
  let rec is_noop = function
    | `Atomic xs -> List.for_all is_noop xs
    | `Dstep xs -> List.for_all is_noop xs
    | `Print _ -> true
    | `Skip -> true
    | `Comment _ -> true
    | _ -> false
  in

  let rec is_simple = function
    | `Atomic xs -> (List.exactly_one is_simple xs) && (List.all_but_one is_noop xs)
    | `Dstep xs -> (List.exactly_one is_simple xs) && (List.all_but_one is_noop xs)
    | `Assign _
    | `Guard _
    | `Run _ -> true
    | _ -> false
  in

  let pack = function | [] -> [] | xs -> [`Atomic xs] in
  let unpack = function | `Atomic xs -> xs | `Dstep xs -> xs | x -> [x] in
  let body', last =
    List.fold_left
      (fun (stmts, fuseable_predecessor) stmt ->
        if is_noop stmt then (* add 'noop' to *)
          (stmts, fuseable_predecessor @ (unpack stmt))
        else if is_simple stmt then
          (stmts @ (pack fuseable_predecessor), unpack stmt)
        else
          (stmts @ (pack (fuseable_predecessor @ (unpack stmt))), [])
        ) ([], []) body
  in
  body' @ (pack last)

(*$T fuse_atomic
  fuse_atomic [`Atomic [`Skip]; `Atomic [`Skip]] = [`Atomic [`Skip;`Skip]]
  fuse_atomic [`Atomic [`Skip]; `Skip] = [`Atomic [`Skip;`Skip]]
  fuse_atomic [`Skip; `Atomic [`Skip]] = [`Atomic [`Skip;`Skip]]
  fuse_atomic [`Skip; `Atomic [`Skip]; `Skip] = [`Atomic [`Skip;`Skip;`Skip]]
  fuse_atomic [`Skip; `Skip;] = [`Atomic [`Skip; `Skip]]
*)


(** normalize the atomic blocks: remove nested blocks and replace all `D_step with `Atomic *)
let reduce_atomic =
  let rec reduce in_atomic stmts =
    (* continue reduction like before *)
    let cont = reduce in_atomic in
    match stmts with
    | (`Atomic [x])::zs when (match x with | `If _ -> false | `IfElse _ -> false | _ -> true) -> (cont [x])@(cont zs)
    | (`Atomic xs)::zs when in_atomic -> (cont xs)@(cont zs)
    | (`Atomic xs)::zs -> (`Atomic (reduce true xs))::(cont zs)
    | (`Dstep xs)::zs when in_atomic -> (cont xs)@(cont zs)
    | (`Dstep xs)::zs -> (`Atomic (reduce true xs))::(cont zs)
    | (`If xss)::zs -> (`If (List.map cont xss))::(cont zs)
    | (`IfElse (xss, ys))::zs -> (`IfElse (List.map cont xss, cont ys))::(cont zs)
    | (`Assign _ as x)::zs -> x::(cont zs)
    | (`Goto _ as x)::zs -> x::(cont zs)
    | (`Label _ as x)::zs -> x::(cont zs)
    | (`Print _ as x)::zs -> x::(cont zs)
    | (`Assert _ as x)::zs -> x::(cont zs)
    | (`Guard _ as x)::zs -> x::(cont zs)
    | (`Run _ as x)::zs -> x::(cont zs)
    | (`Skip as x)::zs -> x::(cont zs)
    | (`Comment _ as x)::zs -> x::(cont zs)
    | [] -> []
  in
  reduce false

(*$T reduce_atomic
  reduce_atomic [`Atomic [`Skip]] = [`Skip]
  reduce_atomic [`Atomic [`Skip; `Skip]] = [`Atomic [`Skip;`Skip]]
  reduce_atomic [`Atomic [`Atomic [`Skip]]] = [`Skip]
  reduce_atomic [`Atomic [`Atomic [`Skip; `Skip]]] = [`Atomic [`Skip;`Skip]]
  reduce_atomic [`Atomic [`Atomic [`Atomic [`Skip]]]] = [`Skip]
  reduce_atomic [`Atomic [`Atomic [`Atomic [`Skip; `Skip]]]] = [`Atomic [`Skip;`Skip]]
  reduce_atomic [`Skip; `Atomic [`Skip]] = [`Skip; `Skip]
  reduce_atomic [`Skip; `Atomic [`Skip]; `Skip] = [`Skip;`Skip;`Skip]
  reduce_atomic [`Atomic [`Skip; `Atomic [`Skip; `Skip]]] = [`Atomic [`Skip; `Skip; `Skip]]
*)


let atomic_section_process p =
  let body = Process.body_of p in
  let body' = reduce_atomic (fuse_atomic (add_atomic body)) in
  Process.body_to p body'

let atomic_section_model =
  Model.map atomic_section_process

