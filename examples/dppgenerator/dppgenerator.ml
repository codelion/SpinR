(* $Id: dppgenerator.ml 4101 2012-09-03 15:38:58Z weissmam $

Copyright (c) 2012 Technische Universitaet Muenchen, TUM
Copyright (c) 2012 Florian Pichlmeier <florian.pichlmeier@in.tum.de>
Copyright (c) 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
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

open ExtLib

exception Not_enough_philosophers of int

let pid tmpl x = Promela.Identifier.create (Printf.sprintf tmpl x)
let fork_id = pid "FORK%d"

(* Definition of one philosopher process *)
let philosopher_process ~number_of_philosophers id =
  let module PE = Promela.Expression in
  let forkl, forkr = id, (id + 1) mod number_of_philosophers in
  let check x = `Guard (PE.Binop (PE.Eq, (PE.Variable (fork_id x)) , (PE.ConstInt 0))) in
  let assign v x = `Assign (fork_id x, None,(PE.ConstInt v)) in
  let take =  assign 1 in
  let release = assign 0 in
  let check_and_take x = `Atomic ((check x)::(take x)::[]) in
  Promela.Process.create ~active:true (pid "PHIL%d" id) [check_and_take forkl; check_and_take forkr; release forkl; release forkr]

(* Dining philosophers problem model with nr philosophers *)
let dining_phil_problem nr =
  let module P = Promela in
  let int_range_of_numbers x =
    let rec aux h = match h with
    | 0 -> []
    | _ -> (h-1)::aux(h-1)
    in
    aux x
  in
  let count = int_range_of_numbers nr in
  let processes = List.map (philosopher_process ~number_of_philosophers:nr) count in
  let declarations = List.fold_left
    (fun decls id -> P.Declarations.add (fork_id id) (P.Type.Bit, Some (P.Expression.ConstInt 0)) decls)
    P.Declarations.empty count
  in
  P.Model.create processes declarations

let _ =
  (* 1st call the command line options parser *)
  let _ = Options.parse_cmdline () in
  let phil_number =  Options.get Options.philosophers in
  let () = Printf.printf "creating model for %d philosophers\n" phil_number in
  let model = if (phil_number >= 2) then dining_phil_problem phil_number else raise (Not_enough_philosophers phil_number) in
  let () = Promela.Model.save (open_out (Options.get Options.output)) model in
  0

