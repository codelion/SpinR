(*_ $Id: process.ml 4527 2012-10-17 13:08:20Z weissmam $

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

type proc = Never | Proc of Identifier.t * bool

type t = {
  id : proc;
  body : Statement.t list;
  locals : Declarations.t;
}

exception Incompatible_process

let header = "PROCESS"

let create ?active:(active=false) ?locals:(l=Declarations.empty) id body =
  { id = Proc (id, active); body = body; locals = l }

let never ?locals:(l=Declarations.empty) body =
  { id = Never; body = body; locals = l }

let id_of p = match p.id with | Never -> Identifier.create "never" | Proc (id, _) -> id 

let is_never p = match p.id with | Never -> true | Proc _ -> false

let body_of p = p.body

let body_to p b = { p with body = b }

let is_active p = match p.id with | Never -> true | Proc (_, true) -> true | _ -> false

let rename_variables f p =
  let body' = Statement.rename_variables_lst f p.body in
  let locals' = Declarations.rename f p.locals in
  {{ p with body = body' } with locals = locals' }

let save oc (p : t) = Serialize.save header oc p

let load ic : t = Serialize.load header Incompatible_process ic

let to_channel oc p =
  let () = set_binary_mode_out oc true in
  let () =
    match p.id with
    | Never -> output_string oc "never"
    | Proc (name, false) -> Printf.fprintf oc "proctype %s()" (Identifier.string_of name)
    | Proc (name, true) -> Printf.fprintf oc "active proctype %s()" (Identifier.string_of name)
  in
  let () = output_string oc " {\n" in
  let () = Declarations.to_channel oc p.locals in
  let () = Statement.to_channel_lst oc p.body in
  let () = Printf.fprintf oc "\n}\n" in
  ()

let locals_of p = p.locals

let locals d p = { p with locals = d }

let identifiers_read p =
  Statement.identifiers_read_lst p.body

let identifiers_written p =
  Statement.identifiers_written_lst p.body

let filter f p =
  { p with body = Statement.filter f p.body }

let map f p =
  { p with body = Statement.map f p.body }

let map_rhs f p =
  { p with body = List.map (Statement.map_rhs f) p.body }

