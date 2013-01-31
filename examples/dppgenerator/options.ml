(* $Id: options.ml 4101 2012-09-03 15:38:58Z weissmam $

Copyright (c) 2010 - 2012 Technische Universitaet Muenchen, TUM
Copyright (c) 2010 - 2012 Markus W. Weissmann <markus.weissmann@in.tum.de>
Copyright (c) 2012 Florian Pichlmeier <florian.pichlmeier@in.tum.de>
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
open OptParse

(* command line options: *)
let output = StdOpt.str_option ~default:"philosophers.pmo" ()
let philosophers = StdOpt.int_option ~default:2 ()

let version : unit Opt.t = StdOpt.version_option (fun () -> Printf.sprintf
"%s
version \"%s\" (%s/ocaml-%s/%s)
Copyright (C) 2010 - 2012 TUM, Chair of Robotics and Embedded Systems\n"
(Filename.basename Sys.argv.(0)) Version.version Sys.os_type Sys.ocaml_version Version.compile_time)

(* init this *)
let options = OptParser.make ()

let parse_cmdline () =
   let open OptParser in
   let () = add options ~short_name:'V' ~long_name:"version" ~help:"return the version" version in
   let () = add options ~short_name:'o' ~long_name:"output" ~help:"name of output file" output in
   let () = add options ~short_name:'p' ~long_name:"philosophers" ~help:"number of philosophers to generate (>= 2)" philosophers in
   OptParse.OptParser.parse_argv options

let get = Opt.get

