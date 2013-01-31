(*_ $Id: expression.ml 4527 2012-10-17 13:08:20Z weissmam $

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

type binary_operator =
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | Lshift
  | Rshift
  | Lt
  | Gt
  | Le
  | Ge
  | Eq
  | Neq
  | Band
  | Bxor
  | Bor
  | And
  | Or

type unary_operator =
  | Neg
  | Not
  | Dash

type t =
  | Binop of binary_operator * t * t
  | Unop of unary_operator * t
  | Variable of Identifier.t
  | ConstInt of int
  | Array of Identifier.t * t

let compare = Pervasives.compare

let replace_variables f expr =
  let rec m =
    function
    | Unop (op, v) -> Unop (op, m v)
    | Binop (op, v1, v2) -> Binop (op, m v1, m v2)
    | Variable id -> f id None
    | ConstInt i -> ConstInt i
    | Array (id, v) -> f id (Some v)
  in
  m expr

let rename_variables f expr =
  let rec rename =
    function
    | Unop (op, v) -> Unop (op, rename v)
    | Binop (op, v1, v2) -> Binop (op, rename v1, rename v2)
    | Variable id -> Variable (f id)
    | ConstInt i -> ConstInt i
    | Array (id, v) -> Array (f id, rename v)
  in
  rename expr

let rec string_of t =
  let string_of_binary_operator =
    function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Band -> "&"
    | Bxor -> "^"
    | Bor -> "|"
    | Gt -> ">"
    | Lt -> "<"
    | Ge -> ">="
    | Le -> "<="
    | Eq -> "=="
    | Neq -> "!="
    | Lshift -> "<<"
    | Rshift -> ">>"
    | And -> "&&"
    | Or -> "||"
  in
  let string_of_unary_operator =
    function
    | Dash -> "~"
    | Neg -> "-"
    | Not -> "!"
  in
  let ret =
    match t with
    | Binop (op, a, b) -> Printf.sprintf "(%s %s %s)" (string_of a) (string_of_binary_operator op) (string_of b)
    | Unop (op, x) -> Printf.sprintf "(%s %s)" (string_of_unary_operator op) (string_of x)
    | Variable id -> Identifier.string_of id
    | ConstInt v -> string_of_int v
    | Array (id, off) -> Printf.sprintf "%s[%s]" (Identifier.string_of id) (string_of off)
  in
  ret

let rec identifiers_of =
  function
  | Binop (_, a, b) -> Identifierset.union (identifiers_of a) (identifiers_of b)
  | Unop (_, x) -> identifiers_of x
  | Variable id -> Identifierset.singleton id
  | ConstInt _ -> Identifierset.empty
  | Array (id, _) -> Identifierset.add id Identifierset.empty

(* simplify expression by computing recursive fixpoint of rewriting rules *)
let rec eval expr =
  let int_of_bool =
    function
    | true -> ConstInt 1
    | false -> ConstInt 0
  in
  let rec try_eval expr =
    match expr with
    | Binop (Eq, x, y) when x = y -> ConstInt 1
    | Binop (Neq, x, y) when x = y -> ConstInt 0
    | Unop (Not, Unop (Not, x)) -> x
    | Unop (Not, ConstInt 1) -> ConstInt 0
    | Unop (Not, ConstInt 0) -> ConstInt 1
    | Binop (Add, ConstInt x, ConstInt y) -> ConstInt (x + y)
    | Binop (Sub, ConstInt x, ConstInt y) -> ConstInt (x - y)
    | Binop (Mul, ConstInt x, ConstInt y) -> ConstInt (x * y)
    | Binop (Div, ConstInt x, ConstInt y) -> ConstInt (x / y)
    | Binop (Mod, ConstInt x, ConstInt y) -> ConstInt (x mod y)
    | Binop (Eq, ConstInt x, ConstInt y) -> int_of_bool (x = y)
    | Binop (Neq, ConstInt x, ConstInt y) -> int_of_bool (x <> y)
    | Binop (Lt, ConstInt x, ConstInt y) -> int_of_bool (x < y)
    | Binop (Le, ConstInt x, ConstInt y) -> int_of_bool (x <= y)
    | Binop (Gt, ConstInt x, ConstInt y) -> int_of_bool (x > y)
    | Binop (Ge, ConstInt x, ConstInt y) -> int_of_bool (x >= y)
    | Binop (op, a, b) -> Binop (op, try_eval a, try_eval b)
    | Unop (op, a) -> Unop (op, try_eval a)
    | _ -> expr
  in
  let expr' = try_eval expr in
  if expr' = expr then expr else eval expr'

(*$Q eval
  (Q.pair Q.int Q.int) (fun (x, y) -> eval (Binop (Add, Binop (Add, ConstInt x, ConstInt y), ConstInt x)) = ConstInt (x + y + x))
  (Q.pair Q.int Q.int) (fun (x, y) -> eval (Binop (Add, ConstInt x, ConstInt y)) = ConstInt (x + y))
  (Q.pair Q.int Q.int) (fun (x, y) -> eval (Binop (Sub, ConstInt x, ConstInt y)) = ConstInt (x - y))
  (Q.pair Q.int Q.int) (fun (x, y) -> eval (Binop (Mul, ConstInt x, ConstInt y)) = ConstInt (x * y))
  Q.int (fun x -> eval (Binop (Eq, ConstInt x, ConstInt x)) = ConstInt 1)
  (Q.pair Q.int Q.int) (fun (x, y) -> eval (Binop (Eq, ConstInt x, ConstInt y)) = ConstInt (if x = y then 1 else 0))
*)
 
(*$T eval
  eval (Unop (Not, Unop (Not, Unop (Not, ConstInt 0)))) = ConstInt 1
*)

