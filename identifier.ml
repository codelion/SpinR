(*_ $Id: identifier.ml 4527 2012-10-17 13:08:20Z weissmam $

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

exception Invalid of string

type t = { v : string }

let check str =
  let identifier = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*" in
  if Str.string_match identifier str 0 then
    str = (Str.matched_string str)
  else
    false

let create str =
  if check str then { v = str } else raise (Invalid str)

(*$T create
  try create "x"; true with (Invalid _) -> false
  try create "X"; true with (Invalid _) -> false
  try create "hello"; true with (Invalid _) -> false
  try create "Hello42"; true with (Invalid _) -> false
  try create "_bla"; true with (Invalid _) -> false
  try create "4a"; false with (Invalid _) -> true
  try create "0"; false with (Invalid _) -> true
  try create "1"; false with (Invalid _) -> true
*)

let string_of id =
  if check id.v then id.v else raise (Invalid id.v)

let compare a b =
  String.compare a.v b.v

