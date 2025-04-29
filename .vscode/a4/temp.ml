(*ast*)
type variable = string
type lamexp =
  | V    of variable
  | App  of lamexp * lamexp
  | Lam  of variable * lamexp
  | Int  of int
  | Bool of bool
  | Plus  of lamexp * lamexp
  | Times of lamexp * lamexp
  | And   of lamexp * lamexp
  | Or    of lamexp * lamexp
  | Not   of lamexp
  | Gt    of lamexp * lamexp
  | Lt    of lamexp * lamexp

(*Church_conversion*)
let church_of_int n =
  let rec aux k e = if k <= 0 then e else aux (k-1) (App (V "f", e)) in Lam ("f", Lam ("x", aux n (V "x")))
let church_true  = Lam ("t", Lam ("f", V "t"))
let church_false = Lam ("t", Lam ("f", V "f"))
let church_zero = Lam("f", Lam("x", V "x"))
let church_one = Lam("f", Lam("x", App(V "f", V "x")))
let church_two = Lam("f", Lam("x", App(V "f", App(V "f", V "x"))))
let church_three = Lam("f", Lam("x", App(V "f", App(V "f", App(V "f", V "x")))))
let church_to_int e =
  let rec aux n e =
    match e with
    | Lam ("f", Lam ("x", body)) ->
        (match body with
         | V "x" -> n
         | App (V "f", remain) -> aux (n + 1) (Lam ("f", Lam ("x", remain)))
         | _ -> failwith "Invalid Church numeral")
    | _ -> failwith "Invalid Church numeral"
  in
  aux 0 e
let church_to_bool e =
  match e with
  | Lam ("t", Lam ("f", body)) ->
      (match body with
       | V "t" -> true
       | V "f" -> false
       | _ -> failwith "Invalid Church boolean")
  | _ -> failwith "Invalid Church boolean"

(*Closure for krivine*)
type gamma = (variable * closure) list
and closure = { exp : lamexp; env : gamma }

(* Krivine(Call-by-Name)machine*)
let rec search x env =
  match env with
  | [] -> { exp = V x; env = [] }
  | (y, clo) :: remain -> if x = y then clo else search x remain

and krivine clo stack =
  let eval_int e =
    match unload (krivine { exp = e; env = clo.env } []) with
    | Int n -> n
    | a -> church_to_int a  
  and eval_bool e =
    match unload (krivine { exp = e; env = clo.env } []) with
    | Bool b -> b
    | b -> church_to_bool b
  in
  match clo.exp, stack with
  | V x, _ -> 
    let result = search x clo.env in
    if result.exp = V x && result.env = [] then
      result  (* It's a free variable, return directly *)
    else
      krivine result stack
  | App (e1, e2), _ ->
      krivine { exp = e1; env = clo.env } ({ exp = e2; env = clo.env } :: stack)
  | Lam (x, e), arg :: remain -> krivine { exp = e; env = (x, arg) :: clo.env } remain
  | Lam (_, _), [] -> clo
  | Int n, [] -> clo 
  | Bool b, [] -> clo
  | Int _, _ -> failwith "Cannot apply Int"
  | Bool _, _ -> failwith "Cannot apply Bool"
  | Plus (a, b), [] -> { exp = church_of_int (eval_int a + eval_int b); env = [] }
  | Times (a, b), [] -> { exp = church_of_int (eval_int a * eval_int b); env = [] }
  | And (a, b), [] ->
      { exp = if eval_bool a && eval_bool b then church_true else church_false; env = [] }
  | Or (a, b), [] ->
      { exp = if eval_bool a || eval_bool b then church_true else church_false; env = [] }
  | Not e, [] -> { exp = if not (eval_bool e) then church_true else church_false; env = [] }
  | Gt (a, b), [] -> { exp = if eval_int a > eval_int b then church_true else church_false; env = [] }
  | Lt (a, b), [] -> { exp = if eval_int a < eval_int b then church_true else church_false; env = [] }
  | _ -> failwith "Krivine Wrong state"

and unload clo =
  match clo.exp with
  | V x ->
    (try let cl = search x clo.env in if cl.exp = V x then V x else unload cl
     with _ -> V x)
  | App (e1, e2) -> App (unload { exp = e1; env = clo.env }, unload { exp = e2; env = clo.env })
  | Lam (x, e) ->
  let env' = List.filter (fun (y, _) -> y <> x) clo.env in
  Lam (x, unload { exp = e; env = env' })
  | Int n -> Int n
  | Bool b -> Bool b
  | Plus (a, b) -> Plus (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })
  | Times (a, b) -> Times (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })
  | And (a, b) -> And (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })
  | Or (a, b) -> Or (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })
  | Not e -> Not (unload { exp = e; env = clo.env })
  | Gt (a, b) -> Gt (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })
  | Lt (a, b) -> Lt (unload { exp = a; env = clo.env }, unload { exp = b; env = clo.env })

(*  SECD Machine (Call-by-Value)  *)
type opcode =
  | LOOKUP of variable
  | MKCLOS of variable * opcode list
  | APP| RET| ADD| MUL| ANDOP| OROP| NOTOP| GTOP| LTOP
  | CONST of value

and value =
  | IntVal of int
  | BoolVal of bool
  | ClosureVal of variable * opcode list * env
  | FreeVal of variable

and env = (variable * value) list

type stack = value list
and code  = opcode list
and dump  = (stack * env * code) list

let rec compile e =
  match e with
  | V x -> [ LOOKUP x ]
  | App (e1, e2) -> compile e2 @ compile e1 @ [ APP ]
  | Lam (x, e1) -> [ MKCLOS (x, compile e1 @ [ RET ]) ]
  | Int n -> [ CONST (IntVal n) ]
  | Bool b -> [ CONST (BoolVal b) ]
  | Plus (a, b) -> compile b @ compile a @ [ ADD ]
  | Times (a, b) -> compile b @ compile a @ [ MUL ]
  | And (a, b) -> compile b @ compile a @ [ ANDOP ]
  | Or (a, b) -> compile b @ compile a @ [ OROP ]
  | Not e -> compile e @ [ NOTOP ]
  | Gt (a, b) -> compile b @ compile a @ [ GTOP ]
  | Lt (a, b) -> compile b @ compile a @ [ LTOP ]

(* church closures for booleans *)
let church_true_clo =
  match compile church_true with
  | [ MKCLOS (x, c) ] -> ClosureVal (x, c, [])
  | _ -> failwith "Invalid church_true_clo"
let church_false_clo =
  match compile church_false with
  | [ MKCLOS (x, c) ] -> ClosureVal (x, c, [])
  | _ -> failwith "Invalid church_false_clo"

let rec run_secd (s, e, c, d) =
  match c with
  | [] -> (match s with v :: _ -> v | [] -> failwith "Empty result")
  | LOOKUP x :: c' ->
      let v = try List.assoc x e with Not_found -> FreeVal x in
      run_secd (v :: s, e, c', d)
  | MKCLOS (x, c1) :: c' ->
      run_secd (ClosureVal (x, c1, e) :: s, e, c', d)
  | APP :: c' ->
      (match s with
       | ClosureVal (x, c1, e1) :: v2 :: s' ->
           run_secd ([], (x, v2) :: e1, c1, (s', e, c') :: d)
       | _ -> failwith "Bad APP")
  | RET :: c' ->
      (match s, d with
       | v :: _, (s', e', c'') :: d' -> run_secd (v :: s', e', c'', d')
       | _ -> failwith "Bad RET")
  | CONST v :: c' -> run_secd (v :: s, e, c', d)
  | ADD :: c' ->
      (match s with
       | IntVal n1 :: IntVal n2 :: s' -> run_secd (IntVal (n2 + n1) :: s', e, c', d)
       | _ -> failwith "ADD")
  | MUL :: c' ->
      (match s with
       | IntVal n1 :: IntVal n2 :: s' -> run_secd (IntVal (n2 * n1) :: s', e, c', d)
       | _ -> failwith "MUL")
  | ANDOP :: c' ->
      (match s with
       | BoolVal b1 :: BoolVal b2 :: s' ->
           let clo = if b2 && b1 then church_true_clo else church_false_clo in
           run_secd (clo :: s', e, c', d)
       | _ -> failwith "ANDOP")
  | OROP :: c' ->
      (match s with
       | BoolVal b1 :: BoolVal b2 :: s' ->
           let clo = if b2 || b1 then church_true_clo else church_false_clo in
           run_secd (clo :: s', e, c', d)
       | _ -> failwith "OROP")
  | NOTOP :: c' ->
      (match s with
       | BoolVal b :: s' ->
           let clo = if not b then church_true_clo else church_false_clo in
           run_secd (clo :: s', e, c', d)
       | _ -> failwith "NOTOP")
  | GTOP :: c' ->
      (match s with
       | IntVal n1 :: IntVal n2 :: s' ->
           let clo = if n2 > n1 then church_true_clo else church_false_clo in
           run_secd (clo :: s', e, c', d)
       | _ -> failwith "GTOP")
  | LTOP :: c' ->
      (match s with
       | IntVal n1 :: IntVal n2 :: s' ->
           let clo = if n2 < n1 then church_true_clo else church_false_clo in
           run_secd (clo :: s', e, c', d)
       | _ -> failwith "LTOP")

(*For Print*)

let rec string_of_value = function
  | IntVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ClosureVal (x, _, e) ->
      let envs = String.concat ", " (List.map fst e) in
      Printf.sprintf "<closure %s [%s]>" x envs
  | FreeVal x -> x

let rec string_of_lamexp = function
  | V x -> x
  | Lam (x, e) -> Printf.sprintf "\\%s.%s" x (string_of_lamexp e)
  | App (a, b) -> Printf.sprintf "(%s %s)" (string_of_lamexp a) (string_of_lamexp b)
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Plus (a, b) -> Printf.sprintf "(%s + %s)" (string_of_lamexp a) (string_of_lamexp b)
  | Times (a, b) -> Printf.sprintf "(%s * %s)" (string_of_lamexp a) (string_of_lamexp b)
  | And (a, b) -> Printf.sprintf "(%s && %s)" (string_of_lamexp a) (string_of_lamexp b)
  | Or (a, b) -> Printf.sprintf "(%s || %s)" (string_of_lamexp a) (string_of_lamexp b)
  | Not e -> Printf.sprintf "(not %s)" (string_of_lamexp e)
  | Gt (a, b) -> Printf.sprintf "(%s > %s)" (string_of_lamexp a) (string_of_lamexp b)
  | Lt (a, b) -> Printf.sprintf "(%s < %s)" (string_of_lamexp a) (string_of_lamexp b)

(*converting secd value to ast*)

let rec lamexp_of_value = function
  | IntVal n -> Int n
  | BoolVal b -> Bool b
  | ClosureVal (x, code, _) ->  
      (match x with
       | "f" -> church_of_int 0 (* For Church 0 *)
       | _ -> 
          (* For other closures, use the original expression if possible *)
           (match x with
            | "t" -> church_true
            | _ -> church_false))
  | FreeVal x -> V x
                   
                   

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

let test_krivine_var_lookup () =
  let env = [("x", { exp = V "x"; env = [] })] in
  let cl = { exp = V "x"; env = env } in
  let result = krivine cl [] in
  assert (unload result = V "x")

let test_krivine_identity_function () =
  let id = Lam ("x", V "x") in
  let env = [("y", { exp = V "y"; env = [] })] in
  let cl = { exp = App (id, V "y"); env = env } in
  let result = krivine cl [] in
  assert (unload result = V "y")

let test_krivine_nested_lambda () =
  let term = App (Lam ("x", Lam ("y", V "x")), V "z") in
  let env = [("z", { exp = V "z"; env = [] })] in
  let cl = { exp = term; env = env } in
  let result = krivine cl [] in
  match unload result with
  | Lam ("y", V "z") -> ()
  | _ -> assert false

let test_krivine_application_of_functions () =
  let f = Lam ("x", App (V "x", V "x")) in
  let arg = Lam ("y", V "y") in
  let env = [] in
  let term = App (f, arg) in
  let cl = { exp = term; env = env } in
  let result = krivine cl [] in
  match unload result with
  | Lam ("y", V "y") -> ()
  | _ -> assert false

let test_krivine_free_variable () =
  let term = Lam ("x", V "y") in
  let env = [("z", { exp = V "z"; env = [] })] in
  let cl = { exp = App (term, V "z"); env = env } in
  let result = krivine cl [] in
  match unload result with
  | V "y" -> ()
  | _ -> assert false

(* ---------- SECD MACHINE TESTS ---------- *)

let test_secd_var_lookup () =
  let env = [("x", FreeVal "x")] in
  let s, e, c, d = ([], env, [LOOKUP "x"], []) in
  let result = run_secd (s, e, c, d) in
  match result with
  | FreeVal "x" -> ()
  | _ -> assert false

let test_secd_simple_function_application () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = run_secd ([], [], prog, []) in
  match result with
  | ClosureVal ("x", [LOOKUP "x"; RET], _) -> ()
  | _ -> assert false

let test_secd_nested_functions () =
  let inner = MKCLOS ("y", [LOOKUP "y"; RET]) in
  let outer = MKCLOS ("x", [inner; RET]) in
  let prog = [outer; outer; APP] in
  let result = run_secd ([], [], prog, []) in
  match result with
  | ClosureVal ("y", [LOOKUP "y"; RET], _) -> ()
  | _ -> assert false

let test_secd_application_ret () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = run_secd ([], [], prog, []) in
  match result with
  | ClosureVal ("x", [LOOKUP "x"; RET], _) -> ()
  | _ -> assert false

let test_secd_closure_environment () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let env = [("y", FreeVal "y")] in
  let result = run_secd ([], env, [lam], []) in
  match result with
  | ClosureVal ("x", [LOOKUP "x"; RET], closure_env) ->
      (* Check that closure_env contains an entry for "y" *)
      let contains_y = List.exists (fun (var, _) -> var = "y") closure_env in
      assert contains_y
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
  test_case "SECD Closure Environment" test_secd_closure_environment;
  