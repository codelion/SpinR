open Promela.Statement 

let _ =
   let module P = Promela in
   let id = P.Identifier.create in
   let d1 = P.Declarations.add (id "A1") (P.Type.Bit,None) P.Declarations.empty in
   let d2 = P.Declarations.add (id "A2") (P.Type.Bit,None) P.Declarations.empty in
   let ds0 = d1 in
   let ds1 = P.Declarations.union d1 d2 in
(*   let ds0 = P.DeclarationSet.singleton d1 in
   let ds1 = P.DeclarationSet.add d1 (P.DeclarationSet.singleton d2) in*)
   let i = id "A1" in
   let e = P.Expression.Variable i in
   let s = `Assign (i, None, e) in
   let a = `Atomic [s;s;s] in
   let p0= P.Process.create ~active:true ~locals:ds0 (id "R0") [a;s;a] in
   let p1 = P.Process.create ~active:true ~locals:ds1 (id "R1") [a;s;a] in
   let ps = P.Model.add (P.Model.add P.Model.empty p0) p1 in
   let () = P.Model.to_channel stdout ps in
	 let oc = open_out_bin "example.in" in
	 let () = P.Model.save oc ps in
   0
