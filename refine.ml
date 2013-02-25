open Statement

type c_exp = [
  | `C_If of c_exp list list
  | `C_IfElse of c_exp list list * c_exp list
	| `C_Seq of c_exp list
  | `C_Assign of Identifier.t * Expression.t option * Expression.t
  | `C_Goto of Label.t
  | `C_Label of Label.t
  | `C_Print of string * Expression.t list
  | `C_Assert of Expression.t
	| `C_Guard of Expression.t
  | `C_Comment of string
	| `C_Skip
	| `C_Lock of Identifier.t
	| `C_Unlock of Identifier.t
	| `C_Call of Identifier.t * Expression.t list
]

type c_func = {
  id : Identifier.t;
  body : c_exp;
  locals : Declarations.t;
}

type c_prog = {
  processes : c_func list;
  globals : Declarations.t;
}

let rec string_of ce =
	let choice lss =
  let cond ls = (Printf.sprintf " (%s) " (string_of (List.hd ls))) ^ (string_of_lst (List.tl ls)) in
  List.fold_left (fun a b -> a ^ (cond b)) "" lss
  in
  match ce with
  | `C_Assign (id, None, ex) ->
    Printf.sprintf "%s = %s" (Identifier.string_of id) (Expression.string_of ex)
  | `C_Assign (id, Some off, ex) ->
    Printf.sprintf "%s[%s] = %s" (Identifier.string_of id) (Expression.string_of off) (Expression.string_of ex)
  | `C_Guard ex -> Printf.sprintf "(%s)" (Expression.string_of ex)
  | `C_Assert ex -> Printf.sprintf "assert(%s)" (Expression.string_of ex)
  | `C_Skip -> ";"	
	| `C_Seq ls -> string_of_lst ls 
	| `C_Lock id -> Printf.sprintf "lock %s" (Identifier.string_of id)
	| `C_Unlock id -> Printf.sprintf "unlock %s" (Identifier.string_of id)
  | `C_Comment str -> Printf.sprintf "/* %s */" str
  | `C_Goto label -> Printf.sprintf "goto %s;" (Label.string_of label)
  | `C_Label label -> Printf.sprintf "%s:" (Label.string_of label)
  | `C_Print (template, exprs) ->
    Printf.sprintf "printf(\"%s\"%s);" template (List.fold_left (fun a b -> Printf.sprintf "%s, %s" a (Expression.string_of b)) "" exprs)
  | `C_If lss -> Printf.sprintf "if %s;" (choice lss)
  | `C_IfElse (lss, es) -> Printf.sprintf "if %s else %s;" (choice lss) (string_of_lst es)
	| `C_Call (p, exprs) ->
    Printf.sprintf "%s(%s);" (Identifier.string_of p) (match exprs with [] -> "" | x::[] -> Expression.string_of x | x::xs -> List.fold_left (fun a b -> a ^ ", " ^ (Expression.string_of b)) (Expression.string_of x) xs)

and string_of_lst = function
  | [] -> ";"
  | x::[] -> string_of x
  | x::xs -> List.fold_left (fun a b -> a ^ ";\n " ^ (string_of b)) (string_of x) xs

let string_of_func cf = Printf.sprintf "%s() \n { %s %s;}\n" (Identifier.string_of cf.id) (Declarations.string_of cf.locals)
													   (string_of cf.body) 

let string_of_prog cp = Printf.sprintf "%s \n %s \n" (Declarations.string_of cp.globals) 
												(String.concat "" (List.map string_of_func cp.processes))

let rec translate_exp (stmt:t):c_exp = 
	match stmt with 
  | `Assign (id, t, ex) ->  `C_Assign(id,t,ex)
  | `Guard ex -> `C_Guard(ex)
  | `Assert ex -> `C_Assert(ex)
  | `Skip -> `C_Skip
  | `Comment str -> `C_Comment(str)
  | `Goto label -> `C_Goto(label)
  | `Label label -> `C_Label(label)
  | `Print (template, exprs) -> `C_Print(template, exprs)
  | `Atomic ls -> let lock_exp = `C_Lock(Identifier.create("l")) in
									let exp_list = List.map translate_exp ls in
									let unlock_exp = `C_Unlock(Identifier.create("l")) in
									`C_Seq([lock_exp]@exp_list@[unlock_exp])
  | `Dstep ls -> let lock_exp = `C_Lock(Identifier.create("l")) in
								 let exp_list = List.map translate_exp ls in
								 let unlock_exp = `C_Unlock(Identifier.create("l")) in
								 `C_Seq([lock_exp]@exp_list@[unlock_exp])
  | `If lss -> let elist = List.map (fun c -> List.map translate_exp c) lss in `C_If(elist) 
  | `IfElse (lss, es) -> let elist = List.map (fun c -> List.map translate_exp c) lss in
												let el = List.map translate_exp es in `C_IfElse(elist,el) 
  | `Run (p, exprs) -> `C_Call(p,exprs)

let translate_process p = {id = (Process.id_of p);
													 body = `C_Seq((List.map (fun c -> translate_exp c) (Process.body_of p)));
													 locals = p.Process.locals}

let translate_model m = { processes = (List.map translate_process m.Model.processes);
														globals = m.Model.globals}