(*_ $Id: promela.mli 4527 2012-10-17 13:08:20Z weissmam $

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

(** Module for creating Promela processes and models as OCaml data structures. *)

(** Representation of Promela identifiers *)
module Identifier :
  sig
    (** A Promela identifier of the form [\[a-zA-Z\]\[a-zA-Z0-9_\]*] *)
    type t

    (** Exception to throw for invalid identifier strings *)
    exception Invalid of string

    (** Create a Promela identifier from a string; if the lexical convention is
       violated, [Invalid] is raised *)
    val create : string -> t

    (** Get the Promela identifier as a string *)
    val string_of : t -> string
  end

(** A set of Promela identifiers *)
module Identifierset : Set.S with type elt = Identifier.t

(** Promela Labels for e.g. jumps *)
module Label :
  sig
    (** A {{:http://spinroot.com/spin/Man/labels.html}Promela label} *)
    type t

    (** Auxiliary attributes of the label *)
    type special = Accept | Progress | End

    (** Create a Promela label from an optional attribute and a Promela identifer *)
    val create : ?special:special -> Identifier.t -> t

    (** Get the string representation of the label;
       this is the same as the Promela identifier including an optional prefix depending on the applied attribute *)
    val string_of : t -> string
  
    val is_end : t -> bool
  end

(** Promela Types for variable declarations *)
module Type :
  sig
    (** The {{:http://spinroot.com/spin/Man/datatypes.html}Promela datatypes} *)
    type t =
      | Bit
      | Byte
      | Short
      | Int
      | Uint
      | Channel
      | Pid

    (** Get the string representation of the type *)
    val string_of : t -> string

    (** [size t] returns the size of the type [t] in bit *)
    val size : t -> int
  end

(** Promela Expressions for e.g. right-hand-sides of assignments *)
module Expression :
  sig
    (** {{:http://www.spinroot.com/spin/Man/operators.html}Promela expressions} *)
    type t =
      | Binop of binary_operator * t * t
      | Unop of unary_operator * t
      | Variable of Identifier.t
      | ConstInt of int
      | Array of Identifier.t * t

    (** {{:http://www.spinroot.com/spin/Man/operators.html}Binary Promela operators} *)
    and binary_operator =
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

    (** {{:http://www.spinroot.com/spin/Man/operators.html}Unary Promela operators} *)
    and unary_operator =
      | Neg
      | Not
      | Dash

    (** [rename_variables f expr] applies the renaming function [f] to all
       variables in [expr] *)
    val rename_variables : (Identifier.t -> Identifier.t) -> t -> t

    (** Get the Promela-compatible string representation of the expression *)
    val string_of : t -> string

    (** [identifiers_of expr] generates a set of all variables occuring in
       [expr] *)
    val identifiers_of : t -> Identifierset.t

    (** [eval expr] tries to evaluate the expression [expr] as far as possible,
       thus returning a potentially simpler expression *)
    val eval : t -> t

    (** [replace_sub sub expr] replaces the expression [sub] in the expression [expr] in a bottom up way. *)
    (* val replace_sub : t -> t -> t *)
  end

(** Promela variable declarations *)
module Declarations :
  sig
    (** Promela variable declarations as a map from identifiers to types and initial values *)
    type t

    (** The key of the declarations map is a Promela identifier *)
    type key = Identifier.t

    (** The value of the declarations map is a tuple of a Promela type and an optional initial value *)
    type value = Type.t * Expression.t option

    exception Type_mismatch of (key * Type.t * Type.t)
    exception Initial_value_mismatch of (key * Expression.t * Expression.t)

    (** create an empty container of declarations *)
    val empty : t

    (** [add id t m] returns a map containing all bindings of [m] plus [id] to [t]. *)
    val add : key -> value -> t -> t

    (** [mem id decls] is a predicate if there is a declaration for the id [id] in the declaration container [decls] *)
    val mem : key -> t -> bool

    (** [remove id decls] creates a new declaration map without [id] *)
    val remove : key -> t -> t
    val find : key -> t -> value
    val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
    val union : t -> t -> t
    val map : (key -> value -> (key * value)) -> t -> t
    val filter : (key -> value -> bool) -> t -> t

    (** Rename all declarations of a set by applying a renaming function to them *)
    val rename : (Identifier.t -> Identifier.t) -> t -> t

    (** generate a string in valid Promela of all declarations in a set *)
    val string_of : t -> string

    (** write all declarations as valid Promlea to a file *)
    val to_channel : out_channel -> t -> unit
  end

(** Promela statements *)
module Statement :
  sig
    (** {{:http://spinroot.com/spin/Man/promela.html#section4}Promela statements} *)
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

    (** generate valid Promela code from a Promela statement *)
    val string_of : t -> string

    (** generate valid Promela code from a list of Promela statements *)
    val string_of_lst : t list -> string

    (** write valid Promela code for a Promela statement to a file, indenting it by [ident] spaces *)
    val to_channel : ?ident:int -> out_channel -> t -> unit

    (** write valid Promela code for a list of Promela statements to a file, indenting it by [ident] spaces *)
    val to_channel_lst : ?ident:int -> out_channel -> t list -> unit

    (** generate a set of all identifiers occuring in a Promela statement *)
    val identifiers_of : t -> Identifierset.t

    (** generate a set of all identifiers that are read by a Promela statement.
      Only functional statements are considered: A [`Printf] always generates an empty set! *)
    val identifiers_read : t -> Identifierset.t

    (** generate a set of all identifiers that are written by Promela statement *)
    val identifiers_written : t -> Identifierset.t

    (** generate a set of all identifiers occuring in a list of Promela statements *)
    val identifiers_of_lst : t list -> Identifierset.t

    (** [rename_variables f stmt] applies the renaming function [f] to all identifiers occuring in [stmt] *)
    val rename_variables : (Identifier.t -> Identifier.t) -> t -> t

    (** [rename_variables f stmts] applies the renaming function [f] to all identifiers occuring in the list [stmts] *)
    val rename_variables_lst :
      (Identifier.t -> Identifier.t) -> t list -> t list

    val filter : (t -> bool) -> t list -> t list
    val map : (t -> t) -> t list -> t list

    (** [map_rhs f stmt] will apply the function [f] to all right-hand-side expressions in the statement [stmt]. *)
    val map_rhs : (Expression.t -> Expression.t) -> t -> t
  end

(** Promela processes *)
module Process :
  sig
    (** {{:http://spinroot.com/spin/Man/proctype.html}Promela process} *)
    type t

    exception Incompatible_process

    (** [create id stmts] creates a Promela process named [id] with a body of [stmts];
      if [locals] and [globals] are omitted, the process will not declare local or global variables itself. *)
    val create :
      ?active:bool ->
      ?locals:Declarations.t -> Identifier.t -> Statement.t list -> t

    val never : ?locals:Declarations.t -> Statement.t list -> t

    val id_of : t -> Identifier.t

    val body_of : t -> Statement.t list

    val body_to : t -> Statement.t list -> t

    val is_active : t -> bool

    val is_never : t -> bool
  
    (** [rename_variables f p] applies the renaming function [f] to all identifiers occuring in the process [p].
       This will rename variables in local and global declarations as in all statements. *)
    val rename_variables : (Identifier.t -> Identifier.t) -> t -> t

    (** [to_channel co p] writes valid Promela code for the given process [p] to channel [co]. *)
    val to_channel : out_channel -> t -> unit

    (** [locals_of p] will return the set of local declarations of the process [p]. *)
    val locals_of : t -> Declarations.t

    (** [locals decls p] will return a new process with the local declarations being [decls] *)
    val locals : Declarations.t -> t -> t

    (** generate a set of all identifiers that are read by the process *)
    val identifiers_read : t -> Identifierset.t

    (** generate a set of all identifiers that are written by a process *)
    val identifiers_written : t -> Identifierset.t

    (** filter all statements in the process *)
    val filter : (Statement.t -> bool) -> t -> t

    (** map all statements in a process *)
    val map : (Statement.t -> Statement.t) -> t -> t

    (** [map_rhs f p] will apply the function [f] to all right-hand-side expressions of the process [p]. *)
    val map_rhs : (Expression.t -> Expression.t) -> t -> t
  end

(** A complete Promela model *)
module Model :
  sig
    (** Promela model *)
    type t

    exception Incompatible_model

    (** create an empty model *)
    val empty : t

    (** create a model from a list of processes and global declarations *)
    val create : Process.t list -> Declarations.t -> t

    (** [add m p] adds the process [p] to the model [m]. *)
    val add : t -> Process.t -> t

    (** [union m1 m2] generates a new model including all processes if [m1] and [m2] and the union of the global declarations of [m1] and [m2]. *)
    val union : t -> t -> t

    (** returns the list of processes of the model *)
    val processes_of : t -> Process.t list 

    (** [to_channel co m] writes valid Promela code for the given model [m] to channel [co]. *)
    val to_channel : out_channel -> t -> unit

    (** [save co m] will generate a binary representation of the model [m] and write it to channel [co]. *)
    val save : out_channel -> t -> unit

    (** [load ci] will load a binary representation of a model from the channel [ci].
       If the given channel does not yield a compatible model, [Incompatible_model] is raised. *)
    val load : in_channel -> t

    (** [rename_variables f m] applies the renaming function [f] to all identifiers occuring in the model [m].
       This will rename variables in local and global declarations as in all statements of all processes. *)
    val rename_variables : (Identifier.t -> Identifier.t) -> t -> t

    (** [size m] computes the state-vector size of the model [m] in bit *)
    val size : t -> int

    (** [globals_of m] will return the set of global declarations of the model [m]. *)
    val globals_of : t -> Declarations.t

    (** [globals decls m] will return a new model with the global declarations being [decls] *)
    val globals : Declarations.t -> t -> t

    (** [add_globals decls m] will return a new model the the global declarations being a union of decls and the previous globals *)
    val add_globals : Declarations.t -> t -> t

    (** generate a set of all identifiers that are read by the model *)
    val identifiers_read : t -> Identifierset.t

    (** generate a set of all identifiers that are written by the model *)
    val identifiers_written : t -> Identifierset.t

    (** filter all statements in the model *)
    val filter : (Statement.t -> bool) -> t -> t

    (** map over all statements of the model *)
    val map_stmt : (Statement.t -> Statement.t) -> t -> t

    (** [map_rhs f m] will apply the function [f] to all right-hand-side expressions of the model [m]. *)
    val map_rhs : (Expression.t -> Expression.t) -> t -> t
  end

(** Normalization functions *)
module Normalization :
  sig
    (** Rewrite atomic sections to remove superfluous atomic sections on a process *)
    val atomic_section_process : Process.t -> Process.t

    (** Rewrite atomic sections to remove superfluous atomic sections on a model *)
    val atomic_section_model : Model.t -> Model.t
  end

