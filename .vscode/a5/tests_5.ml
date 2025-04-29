open Main.Terms

let test_case description f =
  try
    f ();
    ()
  with
  | Assert_failure _ -> Printf.printf "%s failed\n" description


(* ---------- SIGNATURE TESTS ---------- *)
let sig_valid = [("f", 2); ("g", 1); ("h", 0); ("i", 3)] 

let () = test_case "check_sig sig_valid" (fun () -> 
  assert (check_sig sig_valid)
)
let () = test_case "check_sig sig_invalid_arity" (fun () -> 
  let sig_invalid_arity = [("f", -1); ("g", 2)] in
  assert (not (check_sig sig_invalid_arity))
)
let () = test_case "check_sig sig_invalid_duplicate" (fun () -> 
  let sig_invalid_duplicate = [("f", 2); ("f", 3)] in
  assert (not (check_sig sig_invalid_duplicate))
)
let () = test_case "check_sig sig_empty" (fun () -> 
  let sig_empty = [] in
  assert (check_sig sig_empty)
)

(* ---------- WELL-FORMED TERM TESTS ---------- *)

let () = test_case "wfterm t_valid1" (fun () -> 
  let t_valid1 = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  assert (wfterm sig_valid t_valid1)
)
let () = test_case "wfterm t_valid2" (fun () -> 
  let t_valid2 = Node(("i", 3), [| Node(("h", 0), [||]); V "z"; Node(("h", 0), [||]) |]) in
  assert (wfterm sig_valid t_valid2)
)
let () = test_case "wfterm t_invalid1" (fun () -> 
  let t_invalid1 = Node(("f", 2), [| V "x" |])  (* arity mismatch *) in
  assert (not (wfterm sig_valid t_invalid1))
)
let () = test_case "wfterm t_invalid2" (fun () -> 
  let t_invalid2 = Node(("unknown", 1), [| V "x" |])  (* symbol missing *) in
  assert (not (wfterm sig_valid t_invalid2))
)

(* ---------- TERM PROPERTIES TESTS ---------- *)
let t_complex = Node(("f", 2), [|
      Node(("g", 1), [| Node(("g", 1), [| V "x" |]) |]);
    Node(("g", 1), [| Node(("g", 1), [| Node(("g", 1), [| V "y" |]) |]) |])
  |]) 
let sub1 x = if x = "x" then Node(("h", 0), [||]) else V x
let sub2 x = if x = "y" then Node(("h", 0), [||]) else V x 
let t_subbed = subst sub1 t_complex
let compose sub1 sub2 x = subst sub1 (sub2 x)
let comp = compose sub1 sub2

let () = test_case "ht t_complex" (fun () -> 
  assert (ht t_complex = 4)
)
let () = test_case "size t_complex" (fun () -> 
  assert (size t_complex = 8)
)
let () = test_case "vars t_complex" (fun () -> 
  assert (Array.to_list (vars t_complex) = ["x"; "y"])
)

(* ---------- SUBSTITUTION AND COMPOSITION TESTS ---------- *)

let () = test_case "subst sub1 size" (fun () -> 
  assert (size t_subbed = 8)
)

let () = test_case "compose sub1 sub2 on x" (fun () -> 
  match subst comp (V "x") with Node(("h", 0), [||]) -> () | _ -> assert false
)
let () = test_case "compose sub1 sub2 on y" (fun () -> 
  match subst comp (V "y") with Node(("h", 0), [||]) -> () | _ -> assert false
)
let () = test_case "compose sub1 sub2 on z" (fun () -> 
  match subst comp (V "z") with V "z" -> () | _ -> assert false
)

(* ---------- MGU TESTS ---------- *)

let () = test_case "mgu trivial unification" (fun () ->
  let t1 = V "x" and t2 = V "x" in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu unify variable with node" (fun () ->
  let t1 = V "x" in
  let t2 = Node(("h", 0), [||]) in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu failure due to occurs check" (fun () ->
  let t1 = V "x" in
  let t2 = Node(("g", 1), [| V "x" |]) in
  (try let _ = mgu t1 t2 in assert false with NotUnifiable -> ())
)

let () = test_case "mgu non-trivial unification" (fun () ->
  let t1 = Node(("f", 2), [| V "x"; Node(("h", 0), [||]) |]) in
  let t2 = Node(("f", 2), [| Node(("g", 1), [| V "z" |]); V "y" |]) in
  let u = mgu t1 t2 in
  assert (subst u t1 = subst u t2)
)

let () = test_case "mgu failure due to symbol mismatch" (fun () ->
  let t1 = Node(("f", 2), [| V "x"; V "y" |]) in
  let t2 = Node(("g", 2), [| V "x"; V "y" |]) in
  (try let _ = mgu t1 t2 in assert false with NotUnifiable -> ())
)

(* ---------- EDIT TESTS ---------- *)

let () = test_case "edit test" (fun () ->
  let t_editable = Node(("f", 2), [| V "x"; V "y" |]) in
  let edited = edit t_editable [|0|] (Node(("h", 0), [||])) in
  match edited with
  | Node(("f", 2), [| Node(("h", 0), [||]); V "y" |]) -> ()
  | _ -> assert false
)

(* ---------- IN-PLACE SUBSTITUTION TESTS ---------- *)

let () = test_case "inplace_subst test" (fun () ->
  let t = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  let sub x = if x = "x" then Node(("h", 0), [||]) else if x = "y" then Node(("h", 0), [||]) else V x in
  inplace_subst sub t;
  match t with
  | Node(("f", 2), [| Node(("h", 0), [||]); Node(("g", 1), [| Node(("h", 0), [||]) |]) |]) -> ()
  | _ -> assert false
)