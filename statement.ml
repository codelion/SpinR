(*_ $Id: statement.ml 4527 2012-10-17 13:08:20Z weissmam $

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

type t = [
  | `If of t list list
  | `IfElse of t list list * t list
  | `Atomic of t list
  | `Dstep of t list
  | `Assign of Identifier.t * Expression.t option * Expression.t
  | `Goto of Label.t
  | `Label of Label.t
  | `Print of string * Expression.t list
  | `Assert of Expression.t
  | `Guard of Expression.t
  | `Run of Identifier.t * Expression.t list
  | `Skip
  | `Comment of string
]

let rec string_of stmt =
  let choice lss =
    let cond ls = (Printf.sprintf ":: (%s) -> " (string_of (List.hd ls))) ^ (string_of_lst (List.tl ls)) in
    List.fold_left (fun a b -> a ^ (cond b)) "" lss
  in
  match stmt with
  | `Assign (id, None, ex) ->
    Printf.sprintf "%s = %s;" (Identifier.string_of id) (Expression.string_of ex)
  | `Assign (id, Some off, ex) ->
    Printf.sprintf "%s[%s] = %s;" (Identifier.string_of id) (Expression.string_of off) (Expression.string_of ex)
  | `Guard ex -> Printf.sprintf "(%s);" (Expression.string_of ex)
  | `Assert ex -> Printf.sprintf "assert(%s);" (Expression.string_of ex)
  | `Skip -> "skip;"
  | `Comment str -> Printf.sprintf "/* %s */" str
  | `Goto label -> Printf.sprintf "goto %s;" (Label.string_of label)
  | `Label label -> Printf.sprintf "%s:" (Label.string_of label)
  | `Print (template, exprs) ->
    Printf.sprintf "printf(\"%s\"%s);" template (List.fold_left (fun a b -> Printf.sprintf "%s, %s" a (Expression.string_of b)) "" exprs)
  | `Atomic ls -> "atomic { " ^ (string_of_lst ls) ^ " };"
  | `Dstep ls -> "d_step { " ^ (string_of_lst ls) ^ " };"
  | `If lss -> Printf.sprintf "if %s fi;" (choice lss)
  | `IfElse (lss, es) -> Printf.sprintf "if %s :: else -> %s fi;" (choice lss) (string_of_lst es)
  | `Run (p, exprs) ->
    Printf.sprintf "run %s(%s);" (Identifier.string_of p) (match exprs with [] -> "" | x::[] -> Expression.string_of x | x::xs -> List.fold_left (fun a b -> a ^ ", " ^ (Expression.string_of b)) (Expression.string_of x) xs)
and string_of_lst = function
  | [] -> "skip;"
  | x::[] -> string_of x
  | x::xs -> List.fold_left (fun a b -> a ^ "; " ^ (string_of b)) (string_of x) xs

let rec to_channel ?ident:(i=1) oc =
  let rec chan_out ident stmt =
    let prefix = String.make (i * 2) ' ' in
    let default s = Printf.fprintf oc "%s%s" prefix (string_of s) in
    let write_prefix x = Printf.fprintf oc "%s%s" prefix x in
    let write_cond guard_string ls =
      let () = Printf.fprintf oc "\n%s:: %s ->\n" prefix guard_string in
      to_channel_lst ~ident:(i+1) oc ls
    in
    match stmt with
    | `Label label -> Printf.fprintf oc "%s:" (Label.string_of label)
    | `Atomic ls ->
      let () = write_prefix "atomic {\n" in
      let () = to_channel_lst ~ident:(i+1) oc ls in
      Printf.fprintf oc "\n%s};" prefix
    | `Dstep ls ->
      let () = write_prefix "d_step {\n" in
      let () = to_channel_lst ~ident:(i+1) oc ls in
      write_prefix "};\n"
    | `If lss ->
      let () = write_prefix "if" in
      let () = List.iter (fun xs -> write_cond (string_of (List.hd xs)) (List.tl xs)) lss in
      Printf.fprintf oc "\n%sfi;" prefix
    | `IfElse (lss, es) ->
      let () = write_prefix "if" in
      let () = List.iter (fun xs -> write_cond (string_of (List.hd xs)) (List.tl xs)) lss in
      let () = write_cond "else" es in
      Printf.fprintf oc "\n%sfi;" prefix
    | _ -> default stmt
  in
  chan_out 1
and to_channel_lst ?ident:(i=1) oc =
  function
  | [] -> to_channel ~ident:i oc `Skip
  | (`Label lbl)::[] -> let () = to_channel ~ident:i oc (`Label lbl) in to_channel ~ident:i oc `Skip
  | x::[] -> to_channel ~ident:i oc x
  | x::xs ->
    let () = to_channel ~ident:i oc x in
    let () = Printf.fprintf oc "\n" in
    to_channel_lst ~ident:i oc xs

let rec identifiers_of stmt =
  match stmt with
  | `If lss -> identifiers_of_lstlst lss
  | `IfElse (lss, els) -> identifiers_of_lstlst (els::lss)
  | `Atomic ls -> identifiers_of_lst ls
  | `Dstep ls -> identifiers_of_lst ls
  | `Assign (id, None, expr) -> Identifierset.add id (Expression.identifiers_of expr)
  | `Assign (id, Some exa, expr) ->
    Identifierset.add id (Identifierset.union (Expression.identifiers_of expr) (Expression.identifiers_of exa))
  | `Assert expr -> Expression.identifiers_of expr
  | `Guard expr -> Expression.identifiers_of expr
  | `Label _
  | `Skip
  | `Print _
  | `Comment _
  | `Run _
  | `Goto _ -> Identifierset.empty
and identifiers_of_lst stmts =
  List.fold_left (fun a b -> Identifierset.union a (identifiers_of b)) Identifierset.empty stmts
and identifiers_of_lstlst stmtss =
  identifiers_of_lst (List.flatten stmtss)

let rec identifiers_read (stmt : t) =
  match stmt with
  | `If lss -> identifiers_read_lstlst lss
  | `IfElse (lss, els) -> identifiers_read_lstlst (els::lss)
  | `Atomic ls -> identifiers_read_lst ls
  | `Dstep ls -> identifiers_read_lst ls
  | `Assign (id, None, expr) -> Expression.identifiers_of expr
  | `Assign (id, Some exa, expr) ->
    Identifierset.union (Expression.identifiers_of expr) (Expression.identifiers_of exa)
  | `Assert expr -> Expression.identifiers_of expr
  | `Guard expr -> Expression.identifiers_of expr
  | `Label _
  | `Skip
  | `Comment _
  | `Run _
  | `Print _
  | `Goto _ -> Identifierset.empty
and identifiers_read_lst stmts =
  List.fold_left (fun a b -> Identifierset.union a (identifiers_read b)) Identifierset.empty stmts
and identifiers_read_lstlst stmtss =
  identifiers_read_lst (List.flatten stmtss)

let rec identifiers_written (stmt : t) =
  match stmt with
  | `If lss -> identifiers_written_lstlst lss
  | `IfElse (lss, els) -> identifiers_written_lstlst (els::lss)
  | `Atomic ls -> identifiers_written_lst ls
  | `Dstep ls -> identifiers_written_lst ls
  | `Assign (id, None, expr) -> Identifierset.singleton id
  | `Assign (id, Some exa, expr) -> Identifierset.singleton id
  | `Assert expr -> Identifierset.empty
  | `Guard expr -> Identifierset.empty
  | `Label _
  | `Skip
  | `Comment _
  | `Run _
  | `Print _
  | `Goto _ -> Identifierset.empty
and identifiers_written_lst stmts =
  List.fold_left (fun a b -> Identifierset.union a (identifiers_written b)) Identifierset.empty stmts
and identifiers_written_lstlst stmtss =
  identifiers_written_lst (List.flatten stmtss)

let rec rename_variables f (stmt : t) : t =
  let rename_expr = Expression.rename_variables f in
  match stmt with
  | `If lss -> `If (rename_variables_lstlst f lss)
  | `IfElse (lss, els) -> `IfElse (rename_variables_lstlst f lss, rename_variables_lst f els)
  | `Atomic ls -> `Atomic (rename_variables_lst f ls)
  | `Dstep ls -> `Dstep (rename_variables_lst f ls)
  | `Assign (id, None, expr) -> `Assign (f id, None, rename_expr expr)
  | `Assign (id, Some expr1, expr2) -> `Assign (f id, Some (rename_expr expr1), rename_expr expr2)
  | `Guard expr -> `Guard (rename_expr expr)
  | `Assert expr -> `Assert (rename_expr expr)
  | `Print (str, exprs) -> `Print (str, List.map rename_expr exprs)
  | `Skip
  | `Comment _
  | `Label _
  | `Goto _ -> stmt
  | `Run (sub, args) -> `Run (sub, List.map rename_expr args)
and rename_variables_lst f (stmts : t list) : t list =
  List.map (rename_variables f) stmts
and rename_variables_lstlst f (stmtss : t list list) : t list list =
  List.map (rename_variables_lst f) stmtss

let rec filter f stmts =
  let rec filter1 stmt =
    match stmt with
    | `If lss -> `If (List.map (filter f) lss)
    | `IfElse (lss, els) -> `IfElse (List.map (filter f) lss, filter f els)
    | `Atomic ls -> `Atomic (filter f ls)
    | `Dstep ls -> `Dstep (filter f ls)
    | `Assign _
    | `Guard _
    | `Assert _
    | `Print _
    | `Skip
    | `Run _
    | `Comment _
    | `Label _
    | `Goto _ -> stmt
  in
  match stmts with
  | [] -> []
  | stmt::stmts when f stmt -> (filter1 stmt)::(filter f stmts)
  | stmt::stmts -> filter f stmts

let rec map (f : t -> t) =
  function
  | [] -> []
  | stmt::stmts ->
    let stmt' =
      begin
        match stmt with
        | `If lss -> f (`If (List.map (map f) lss))
        | `IfElse (lss, els) -> f (`IfElse (List.map (map f) lss, map f els))
        | `Atomic ls -> f (`Atomic (map f ls))
        | `Dstep ls -> f (`Dstep (map f ls))
        | `Label _
        | _ -> f stmt
      end
    in
    stmt'::(map f stmts)

let rec map_rhs f stmt =
  let m = List.map (map_rhs f) in
  let mm = List.map m in
  match stmt with
  | `If lss -> `If (mm lss)
  | `IfElse (lss, es) -> `IfElse (mm lss, m es)
  | `Atomic ls -> `Atomic (m ls)
  | `Dstep ls -> `Dstep (m ls)
  | `Goto _ -> stmt
  | `Label _
  | `Comment _
  | `Skip -> stmt
  | `Run (id, exprs) -> `Run (id, List.map f exprs)
  | `Guard expr -> `Guard (f expr)
  | `Assign (id, None, expr) -> `Assign (id, None, f expr)
  | `Assign (id, Some offset, expr) -> `Assign (id, Some (f offset), f expr)
  | `Assert expr -> `Assert (f expr)
  | `Print (template, exprs) -> `Print (template, List.map f exprs)

