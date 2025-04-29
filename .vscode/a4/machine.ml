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
  | V x, _ -> krivine (search x clo.env) stack
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

(*  Tests   *)
let succ = Lam("n", Lam("f", Lam("x", App(V "f", App(App(V "n", V "f"), V "x")))))
let add = Lam("m", Lam("n", Lam("f", Lam("x", App(App(V "m", V "f"), App(App(V "n", V "f"), V "x"))))))
let mult = Lam("m", Lam("n", Lam("f", App(V "m", App(V "n", V "f")))))
let pred = Lam("n", Lam("f", Lam("x",
                                 App(App(App(V "n",
                                             Lam("g", Lam("h", App(V "h", App(V "g", V "f"))))),
                                         Lam("u", V "x")),
                                     Lam("u", V "u")))))


let church_and = Lam("p", Lam("q", App(App(V "p", V "q"), V "p")))
let church_or = Lam("p", Lam("q", App(App(V "p", V "p"), V "q")))
let church_not = Lam("p", Lam("a", Lam("b", App(App(V "p", V "b"), V "a"))))

let y_comb = Lam("f", App(Lam("x", App(V "f", App(V "x", V "x"))),
                          Lam("x", App(V "f", App(V "x", V "x")))))

let test_succ = App(succ, church_one)
let test_add = App(App(add, church_one), church_two)
let test_mult = App(App(mult, church_two), church_three)
let test_pred = App(pred, church_three)
let test_and = App(App(church_and, church_true), church_false)
let test_or = App(App(church_or, church_false), church_true)
let test_not = App(church_not, church_false)
let cons = Lam("a", Lam("b", Lam("m", App(App(V "m", V "a"), V "b"))))
let car = Lam("p", App(V "p", church_true))
let cdr = Lam("p", App(V "p", church_false))
let pair_ab = App(App(cons, V "a"), V "b")
let test_car = App(car, pair_ab)
let test_cdr = App(cdr, pair_ab)
let if_true_expr = App(App(church_true, church_two), church_three)
let if_false_expr = App(App(church_false, church_two), church_three)
let tests = [
  (*Pure lambda tests*)
  ("Church 0", Lam ("f", Lam ("x", V "x")));
  ("Church 1", Lam ("f", Lam ("x", App (V "f", V "x"))));
  ("Church 2", Lam ("f", Lam ("x", App (V "f", App (V "f", V "x")))));
  ("Successor 1->2", App(succ, church_one));
  ("Addition 1+2", App(App(add, church_one), church_two));
  ("Multiplication 2*3", App(App(mult, church_two), church_three));
  ("Predecessor 3->2", App(pred, church_three));
  ("Church AND false&&true", App(App(church_and, church_true), church_false));
  ("Church OR false||true", App(App(church_or, church_false), church_true));
  ("Church NOT false",App(church_not, church_false));
  ("id_int", App (Lam ("x", V "x"), Int 42));
  ("id_bool", App (Lam ("x", V "x"), Bool true));
  ("const42", App (Lam ("x", Lam ("y", Int 42)), Bool false));
  (* Primitive tests *)
  ("2+3", Plus (Int 2, Int 3));
  ("4*5", Times (Int 4, Int 5));
  ("true&&false", And (Bool true, Bool false));
  ("true||false", Or (Bool true, Bool false));
  ("not false", Not (Bool false));
  ("5>3", Gt (Int 5, Int 3));
  ("2<7", Lt (Int 2, Int 7)); 
  ("test7",App (
    App (
      Lam ("x", Lam ("y", (Plus( V "x", V "y")))),
      Int 3
    ),
    Int 4
  ));
  ("plus 1+2+3+0+3", Plus(Plus (Plus (Int 1, Int 2), Plus (Int 3, Int 0)),Int 3)); 
]
let rec string_of_opcode = function
  | LOOKUP x        -> Printf.sprintf "LOOKUP(%s)" x
  | MKCLOS(x, code) -> Printf.sprintf "MKCLOS(%s, [%s])"
                         x (string_of_opcode_list code)
  | APP             -> "APP"
  | RET             -> "RET"
  | CONST v ->
      let inner = match v with
        | IntVal n          -> string_of_int n
        | BoolVal b         -> string_of_bool b
        | ClosureVal(x,_,_) -> Printf.sprintf "<closure %s>" x
        | FreeVal x         -> x
      in
      "CONST(" ^ inner ^ ")"
  | ADD             -> "ADD"
  | MUL             -> "MUL"
  | ANDOP           -> "ANDOP"
  | OROP            -> "OROP"
  | NOTOP           -> "NOTOP"
  | GTOP            -> "GTOP"
  | LTOP            -> "LTOP"

and string_of_opcode_list ops =
  String.concat "; " (List.map string_of_opcode ops)

(*Main*)
let () =
  Printf.printf "Running both machines:\n\n";
  List.iter (fun (nm, exp) -> 
    let kr_clo = krivine { exp; env = [] } [] in
    let kr_res = unload kr_clo in
    let kr_str = string_of_lamexp kr_res in
 
    let sc_val = run_secd ([], [], compile exp, []) in
    let sc_str = match sc_val with
      | IntVal n        -> string_of_int n
      | BoolVal b       -> string_of_bool b
      | ClosureVal(x, c, _) -> 
          Printf.sprintf "<closure %s [%s]>" x (string_of_opcode_list c)
      | FreeVal x       -> x
    in
    Printf.printf "Expression: %s\n" (string_of_lamexp exp);
    Printf.printf "  Krivine: %s\n" kr_str;
    Printf.printf "  SECD: %s\n\n" sc_str
  ) tests
