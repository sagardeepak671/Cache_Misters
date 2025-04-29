(* ---------- TEST FRAMEWORK ---------- *)

let test_case name f =
  try
    f ();
    Printf.printf "Test '%s' passed!\n" name
  with
  | Assert_failure _ ->
      Printf.printf "Test '%s' FAILED (assertion failure)!\n" name
  | e ->
      Printf.printf "Test '%s' FAILED with exception: %s\n" name (Printexc.to_string e)

(* ---------- KRIVINE MACHINE TESTS ---------- *)

open Krivine  (* Assuming your krivine.ml exposes Krivine module *)

let test_krivine_var_lookup () =
  let env = [Entry ("x", Clos (V "x", []))] in
  let cl = Clos (V "x", env) in
  let result = krivine_exec cl [] in
  assert (unload result = V "x")

let test_krivine_identity_function () =
  let id = Lam ("x", V "x") in
  let env = [Entry ("y", Clos (V "y", []))] in
  let cl = Clos (App (id, V "y"), env) in
  let result = krivine_exec cl [] in
  assert (unload result = V "y")

let test_krivine_nested_lambda () =
  let term = App (Lam ("x", Lam ("y", V "x")), V "z") in
  let env = [Entry ("z", Clos (V "z", []))] in
  let cl = Clos (term, env) in
  let result = krivine_exec cl [] in
  match unload result with
  | Lam ("y", V "z") -> ()
  | _ -> assert false

let test_krivine_application_of_functions () =
  let f = Lam ("x", App (V "x", V "x")) in
  let arg = Lam ("y", V "y") in
  let env = [] in
  let term = App (f, arg) in
  let cl = Clos (term, env) in
  let result = krivine_exec cl [] in
  match unload result with
  | Lam ("y", V "y") -> ()
  | _ -> assert false

let test_krivine_free_variable () =
  let term = Lam ("x", V "y") in
  let env = [Entry ("z", Clos (V "z", []))] in
  let cl = Clos (App (term, V "z"), env) in
  let result = krivine_exec cl [] in
  match unload result with
  | V "y" -> ()
  | _ -> assert false

(* ---------- SECD MACHINE TESTS ---------- *)

open Secd  (* Assuming your secd.ml exposes SECD module *)

let test_secd_var_lookup () =
  let env = [ENTRY ("x", VCLOS ("x", [], []))] in
  let s, e, c, d = ([], env, [LOOKUP "x"], []) in
  let result = secd_execute s e c d in
  match result with
  | VCLOS ("x", [], _)  -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_simple_function_application () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("x", [LOOKUP "x"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_nested_functions () =
  let inner = MKCLOS ("y", [LOOKUP "y"; RET]) in
  let outer = MKCLOS ("x", [inner; RET]) in
  let prog = [outer; outer; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("y", [LOOKUP "y"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_application_ret () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_execute [] [] prog [] in
  match result with
  | VCLOS ("x", [LOOKUP "x"; RET], _) -> ()  (* Checking for the closure *)
  | _ -> assert false

  let test_secd_closure_environment () =
    let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
    let env = [ENTRY ("y", VCLOS ("y", [], []))] in
    let s, e, c, d = ([], env, [lam], []) in
    let result = secd_execute s e c d in
    match result with
    | VCLOS ("x", [LOOKUP "x"; RET], closure_env) ->
        (* Check that closure_env contains an ENTRY for "y" *)
        let rec contains_y = function
          | ENTRY ("y", _) :: _ -> true
          | _ :: rest -> contains_y rest
          | [] -> false
        in
        assert (contains_y closure_env)  (* Check for closure environment *)
    | _ -> assert false
  
(* ---------- RUN ALL TESTS ---------- *)
 

let () =
  (* Krivine Tests *)
  test_case "Krivine Var Lookup" test_krivine_var_lookup;
  test_case "Krivine Identity Function" test_krivine_identity_function;
  test_case "Krivine Nested Lambda" test_krivine_nested_lambda;
  test_case "Krivine Application of Functions" test_krivine_application_of_functions;
  test_case "Krivine Free Variable" test_krivine_free_variable;

  (* SECD Tests *)
  test_case "SECD Var Lookup" test_secd_var_lookup;
  test_case "SECD Simple Function Application" test_secd_simple_function_application;
  test_case "SECD Nested Functions" test_secd_nested_functions;
  test_case "SECD Application RET" test_secd_application_ret;
  test_case "SECD Closure Environment" test_secd_closure_environment 
