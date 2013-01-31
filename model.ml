(*_ $Id: model.ml 4527 2012-10-17 13:08:20Z weissmam $

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

type t = {
  processes : Process.t list;
  globals : Declarations.t;
}

exception Incompatible_model

let header = "MODEL"

let create ps decls = {
  processes = ps;
  globals = decls;
}

let empty = create [] Declarations.empty

let globals decls m = { m with globals = decls }

let globals_of m = m.globals

let add_globals decls m =
  globals (Declarations.union (globals_of m) decls) m

let processes ps m = { m with processes = ps }

let processes_of m = m.processes

let add m p =
  { m with processes = (processes_of m) @ [p] }

let union m1 m2 =
  let m = processes ((processes_of m1) @ (processes_of m2)) m1 in
  add_globals (globals_of m2) m

let to_channel oc m =
  let () = set_binary_mode_out oc true in
  let () = Declarations.to_channel oc (globals_of m) in
  let () = List.iter (Process.to_channel oc) m.processes in
  ()

let rename_variables f m =
  let ps = List.map (Process.rename_variables f) (processes_of m) in
  let decls = Declarations.rename f (globals_of m) in
  processes ps (globals decls m)

let size m =
  let s = Declarations.fold (fun _ (t, _) c -> (Type.size t) + c) (globals_of m) 0 in
  s

let identifiers_read m =
  List.fold_left (fun vs p -> Identifierset.union vs (Process.identifiers_read p)) Identifierset.empty m.processes

let identifiers_written m =
  List.fold_left (fun vs p -> Identifierset.union vs (Process.identifiers_written p)) Identifierset.empty m.processes

let filter f m =
  { m with processes = List.map (fun p -> Process.filter f p) m.processes }

let map f m =
  { m with processes = List.map f m.processes }

let map_stmt f m =
  map (fun p -> Process.map f p) m

let save oc (m : t) = Serialize.save header oc m

let load ic : t = Serialize.load header Incompatible_model ic

let map_rhs f m =
  { m with processes = List.map (Process.map_rhs f) m.processes }

