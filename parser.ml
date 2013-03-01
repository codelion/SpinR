open Camlp4
open PreCast
open Promela
open Promela.Statement
module M = Camlp4.PreCast

let model = Gram.Entry.mk "model"

EXTEND Gram
  GLOBAL: model;
	
	model: [[ g = LIST0 decl;p = LIST1 process -> if (List.length g == 0)  then Model.create p Declarations.empty
						    else Model.create p (List.fold_left (fun a b -> Declarations.union a b) Declarations.empty g) 
				 ]];
	process: [[ "active" ; "proctype" ; `UIDENT(s); "(" ; ")" ; "{" ; l= LIST0 decl; b = LIST1 statement;"}" ->
													if (List.length l == 0) then 
													let ld = List.fold_left (fun a b -> Declarations.union a b) Declarations.empty l in
													Process.create ~active:true ~locals:ld (Identifier.create s) b
												  else Process.create ~active:true ~locals:Declarations.empty (Identifier.create s) b
					|		"proctype" ; `UIDENT(s); "(" ; ")" ; "{" ; l= LIST0 decl; b = LIST1 statement;"}" -> 
													if (List.length l == 0) then 
													let ld = List.fold_left (fun a b -> Declarations.union a b) Declarations.empty l in
													Process.create ~active:false ~locals:ld (Identifier.create s) b
												  else Process.create ~active:true ~locals:Declarations.empty (Identifier.create s) b													
						]]; 
		
	decl: [[ "bit"; `UIDENT(s); OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Bit,e) Declarations.empty 
	      |  "byte"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Byte,e) Declarations.empty
				|  "short"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Short,e) Declarations.empty
				|  "int"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Int,e) Declarations.empty
				|  "unsigned"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Uint,e) Declarations.empty
				|  "chan"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Channel,e) Declarations.empty
				|  "pid"; `UIDENT(s);OPT "=";e = OPT expression -> Declarations.add (Identifier.create s) (Type.Pid,e) Declarations.empty
				]];
	
	choice:[[
			"::"; s1 = statement; "->"; s2=statement -> [s1]@[s2]
		]];
	
	statement:[[
			"if"; chst = LIST1 choice ;"fi" ; ";" -> `If(chst)
		| "if"; chst = LIST1 choice ; "::" ; "else"; "->" ; ls = LIST1 SELF;"fi"; ";" -> `IfElse(chst,ls)
		|	"atomic";"{"; st = LIST1 SELF ;"}"; ";" -> `Atomic(st)
		| "d_step";"{"; st = LIST1 SELF ;"}"; ";" -> `Dstep(st)
		| `UIDENT(s); "=" ; e = expression; ";" -> `Assign((Identifier.create s),None,e)
		| "printf";"("; `STRING(s,_); el = LIST1 expression ;")"; ";" -> `Print(s,el) 
		| "assert";"("; e = expression; ")"; ";" -> `Assert(e)
		| "("; e = expression; ")"; ";" -> `Guard(e)
		| "run"; `LIDENT(s) ;"(";el = LIST0 expression; ")" ; ";" -> `Run((Identifier.create s),el)
		| "/*"; `STRING(s,_) ;"*/" -> `Comment(s)		
		| "skip" ; ";" -> `Skip
		]];
  expression:
       [ LEFTA  
       	 [e1 = SELF;"||"; e2 = SELF -> Expression.Binop(Expression.Or,e1,e2)]
	| LEFTA
	  [e1 = SELF;  "&&";e2 = SELF -> Expression.Binop(Expression.And,e1,e2)]
 	|   "Equality" LEFTA 
            [e1 = SELF; "==";e2 = SELF -> Expression.Binop(Expression.Eq,e1,e2) 
						| e1 = SELF; "!=";e2 = SELF -> Expression.Binop(Expression.Neq,e1,e2)]
        |  "Compare" LEFTA
           [e1 = SELF; "<";e2 = SELF -> Expression.Binop(Expression.Lt,e1,e2) 
        |   e1 = SELF;  ">";e2 = SELF -> Expression.Binop(Expression.Gt,e1,e2) ]
        |  "Sub Add" LEFTA
          [ e1 = SELF;"-";  e2 = SELF -> Expression.Binop(Expression.Add,e1,e2) 
          | e1 = SELF; "+"; e2 = SELF -> Expression.Binop(Expression.Sub,e1,e2)  ]
        | "Mul Div" LEFTA
          [ e1 = SELF; "*"; e2 = SELF -> Expression.Binop(Expression.Mul,e1,e2) 
          | e1 = SELF; "/";  e2 = SELF -> Expression.Binop(Expression.Div,e1,e2)  ]
        | "Bracket" LEFTA
           ["("; e=SELF; ")" -> e]
        | "Unary" NONA
          [ "~"; e=SELF -> Expression.Unop(Expression.Neg,e)
          | "not"; e=SELF -> Expression.Unop(Expression.Not,e)
					| "-"; e=SELF -> Expression.Unop(Expression.Dash,e)
          ]
        | "Constant" NONA 
          [ `INT (i, _) -> Expression.ConstInt(i)
					| `UIDENT(i) -> Expression.Variable((Identifier.create i))
          ]
      ];
  END

let _loc = Loc.mk "<string>" 

(* parse from a string *)
let parse fname str =
try 
  let e = Gram.parse_string model _loc str in e 
with
      End_of_file -> exit 0
    | M.Loc.Exc_located (l,t)->
      (print_string ("Parse Error in "^(Camlp4.PreCast.Loc.to_string l)^"\n");
       raise t)

	(* parse from a file *)
let parse_file (filename:string) : (string * Model.t) =
  let contents =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file ->
        close_in chan;List.rev !lines in
  let str = String.concat "" contents in
  let chan = open_in filename in
  let istream = Stream.of_channel chan in
  try 
    let e = Gram.parse model (PreCast.Loc.mk filename) istream in
    (str,e)
  with M.Loc.Exc_located (l,t)->
      (print_string ("Parse Error in "^(Camlp4.PreCast.Loc.to_string l)^"\n");
       raise t)