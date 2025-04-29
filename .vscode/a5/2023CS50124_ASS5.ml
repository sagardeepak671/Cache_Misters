module Terms = struct
  type variable = string
  type symbol = string * int  (* symbol name * arity *)

  type term =
    | V of variable
    | Node of symbol * term array

  exception WRONG_SIGNATURE of string
  exception NOT_UNIFIABLE

  (*1=>
  Given a signature consisting of symbols with their arities (>= 0) 
in any suitable form -- either as a list of (symbol, arity) pairs, 
or as a function from strings to non-negative integer arities, 
write a function check_sig that checks whether the signature is a 
valid signature (no repeated symbols, arities are non-negative etc.)
   *)
  let check_sig (sigs : (string * int) list) : bool =
    let table = Hashtbl.create (List.length sigs) in
    List.for_all (fun (sym, ar) ->
        if ar < 0 || Hashtbl.mem table sym then false
        else (
          Hashtbl.add table sym ar; 
          true
          )
      ) sigs

  (*2=> 
  Given a valid signature (checked using check_sig), 
  define a function wfterm that checks that a given preterm is 
  well-formed according to the signature, i.e., every node labelled 
  by a symbol has exactly as many subterms as specified by the arity.
  *)
  let wfterm (sigs : (string * int) list) (tree : term) : bool =
    if not (check_sig sigs) then raise (WRONG_SIGNATURE "Invalid signature")
    else
      let table = Hashtbl.create (List.length sigs) in
      List.iter (fun (n,a) -> Hashtbl.add table n a) sigs;
      let rec wfterm_rec x = match x with
        | V _ -> true
        | Node ((n,a), arr) ->
            (match Hashtbl.find_opt table n with
             | Some expected when expected = a && Array.length arr = a ->
                 Array.for_all wfterm_rec arr
             | _ -> false)
      in wfterm_rec tree

  (*3=>
  Define functions ht, size and vars that given a well-formed term,
  return its height, the number of nodes in it, and the set of variables 
  appearing in it respectively.  Use the functions from the Array module: 
   map, foldl and other such functions as far as possible wherever you use lists.  
  *)
  let ht (t : term) : int =
    let rec ht_rec = function
      | V _ -> 0
      | Node (_, arr) ->
          if Array.length arr = 0 then 0
          else 1 + Array.fold_left max 0 (Array.map ht_rec arr)
    in ht_rec t

  let size (t : term) : int =
    let rec size_rec = function
      | V _ -> 1
      | Node (_, arr) ->
          1 + Array.fold_left (+) 0 (Array.map size_rec arr)
    in size_rec t

  module VarSet = Set.Make(String)

  let set_to_list s =
    VarSet.fold (fun v acc -> v :: acc) s []

  let vars (t : term) : variable list = 
    let rec vars_rec term acc =
      match term with
      | V v -> if VarSet.mem v acc then acc else VarSet.add v acc
      | Node (_, arr) ->
          Array.fold_left (fun s child -> vars_rec child s) acc arr
    in 
    let all_vars = vars_rec t VarSet.empty in
    set_to_list all_vars


  (*4=>
  Define a suitable representation for substitutions.  
  *)
  type substitution = (variable * term) list

  (*5=>
  Define the function subst that given a term t and a substitution s, 
  applies the (Unique Homomorphic Extension of) s to t. 
  Ensure that subst is efficiently implemented. 
  *)
  let rec subst (t : term) (s : substitution) : term =
    let subst_table = Hashtbl.create (List.length s) in
    List.iter (fun (v, u) -> Hashtbl.add subst_table v u) s;
    let rec subst_rec t = match t with
      | V v -> (match Hashtbl.find_opt subst_table v with Some u -> u | None -> t)
      | Node (sym, arr) ->
          Node (sym, Array.map subst_rec arr)
    in
    subst_rec t

  (*6=> 
  Come up with an efficient representation of composition of substitutions. 
  *)
  let compose_subst (s1 : substitution) (s2 : substitution) : substitution =
    let mapped = List.map (fun (v,u) -> (v, subst u s2)) s1 in
    let keys1 = List.map fst s1 in
    let extra = List.filter (fun (v,_) -> not (List.mem v keys1)) s2 in
    mapped @ extra

  (*7=>
  Define the function mgu that given two terms t1 and t2, 
  returns their most general unifier, if it exists and 
    otherwise raises an exception NOT_UNIFIABLE.
  *) 

(* check variable in tree*)
  let check_presence (v : variable) (tree : term) : bool =
    let rec check_rec = function
      | V y -> y = v
      | Node (_, children) -> Array.exists check_rec children
    in check_rec tree
  
  (* applying substitution to pair of trees *)
  let apply_subst_pair s (a, b) = (subst a s, subst b s)
  
  let update_equations x t equations =
    List.map (apply_subst_pair [(x, t)]) equations
  
  let decompose_nodes f1 a1 f2 a2 =
    if f1 <> f2 || Array.length a1 <> Array.length a2 then raise NOT_UNIFIABLE
    else Array.to_list (Array.mapi (fun i t1 -> (t1, a2.(i))) a1)
  
  (* mgu helper *)
  let rec unify_pairs subst_env = function
    | [] -> subst_env
    | (u, v) :: rest ->
        let u' = subst u subst_env in
        let v' = subst v subst_env in
        if u' = v' then unify_pairs subst_env rest
        else match u', v' with
          | V x, t when not (check_presence x t) ->
              let new_env = (x, t) :: subst_env in
              let new_rest = update_equations x t rest in
              unify_pairs new_env new_rest
          | t, V x ->
              unify_pairs subst_env ((V x, t) :: rest)
          | Node (f1, a1), Node (f2, a2) ->
              let decomposed = decompose_nodes f1 a1 f2 a2 in
              unify_pairs subst_env (decomposed @ rest)
          | _ -> raise NOT_UNIFIABLE
  
  let mgu (t1 : term) (t2 : term) : substitution =
    unify_pairs [] [(t1, t2)]

  (*8=>
  Define an efficient edit function, that given a legal 
  position in a tree, replaces the given subtree at that 
  position by another specified tree
  *)
  let edit (t : term) (path : int list) (replacement : term) : term =
    let rec edit_rec current = function
      | [] -> replacement
      | i::ps ->
          (match current with
           | Node (sym, arr) when i >= 0 && i < Array.length arr ->
               let arr' =
                 Array.mapi (fun j u ->
                     if j = i then edit_rec u ps else u
                   ) arr
               in Node(sym, arr')
           | _ -> invalid_arg "Invalid position")
    in edit_rec t path

  (*9=> 
  Define also an in-place substitution operation that replaces
   variables by given terms in situ (modifying the terms by 
   replacing the free variable occurrences in a term). 
  *)
  let inplace_subst (t : term) (s : substitution) : term =
    let subst_table = Hashtbl.create (List.length s) in
    List.iter (fun (v, u) -> Hashtbl.add subst_table v u) s;
    let rec inplace_subst_rec t = match t with
      | V v -> (match Hashtbl.find_opt subst_table v with Some u -> u | None -> t)
      | Node (sym, arr) ->
          Array.iteri (fun i u -> arr.(i) <- inplace_subst_rec u) arr;
          t
    in
    inplace_subst_rec t

  (*--- Helper utilities ---*)

  let make_node (name:string) (children:term list) : term =
    Node((name, List.length children), Array.of_list children)

  let rec string_of_term (t : term) : string = match t with
    | V v -> v
    | Node ((n,a), arr) ->
        if a = 0 then n
        else
          let parts = Array.to_list arr |> List.map string_of_term in
          n ^ "(" ^ String.concat "," parts ^ ")"
end

open Terms

(*==================== Test Suite ====================*)
let () =
  let test name cond =
    if cond then
      Printf.printf "[ OK ] %s\n" name
    else begin
      Printf.eprintf "[FAIL] %s\n" name;
      failwith (Printf.sprintf "Test '%s' failed" name)
    end
  in

  (* Basic tests *)
  test "check_sig valid"       (check_sig [("f",2); ("a",0)]);
  test "check_sig duplicate"   (not (check_sig [("x",1); ("x",1)]));
  let sig1 = [("f",2); ("a",0)] in
  let t1 = Node(("f",2), [|V"x"; Node(("a",0), [||])|]) in
  test "wfterm simple"         (wfterm sig1 t1);
  test "ht simple"             (ht t1 = 1);
  test "size simple"           (size t1 = 3);
  test "vars simple"           (vars t1 = ["x"]);

  let t2 = subst t1 [("x", Node(("a",0), [||]))] in
  let ok_subst = match t2 with
    | Node((_,_), [| Node(("a",0), _); _ |]) -> true
    | _ -> false
  in test "subst simple"       ok_subst;

  let s1 = [("x",V"y")] and s2 = [("y",V"z")] in
  let sc = compose_subst s1 s2 in
  test "compose_subst simple"  (List.mem ("x",V"z") sc && List.mem ("y",V"z") sc);

  let t3 = Node(("f",1), [|V"x"|]) in
  let t4 = Node(("f",1), [|Node(("a",0),[||])|]) in
  let theta = mgu t3 t4 in
  test "mgu simple"           (List.mem ("x", Node(("a",0),[||])) theta);

  let t5 = Node(("g",2), [|V"u";V"v"|]) in
  let t6 = edit t5 [1] (V"w") in
  let ok_edit = match t6 with
    | Node(_, [| _; V"w" |]) -> true
    | _ -> false
  in test "edit simple"       ok_edit;

  let t7 = Node(("h",1), [|V"p"|]) in
  let t7' = inplace_subst t7 [("p",V"q")] in
  let ok_inplace = match t7' with
    | Node(_, [|V"q"|]) -> true
    | _ -> false
  in test "inplace simple"     ok_inplace;

  (* Larger test cases *)
  let big = make_node "f" [
      make_node "g" [V"x"; make_node "a" []];
      make_node "h" [V"y"; V"z"; make_node "b" []];
      make_node "k" [make_node "m" [V"u"]]
    ] in
  test "big ht"               (ht big = 3);
  test "big size"             (size big = 11);
  test "big vars"             (List.sort compare (vars big) = ["u";"x";"y";"z"]);
  Printf.printf "Big term: %s\n" (string_of_term big);

  (* Deep substitution chain *)
  let chain = [
    ("x", make_node "g" [V"y"]);
    ("y", make_node "h" [V"z"]);
    ("z", V"w");
  ] in
  test "deep chain"           (string_of_term (subst (V"x") chain) = "g(y)");

  (* Compose with extras *)
  let s1 = [("x",V"y"); ("y",V"z")] in
  let s2 = [("z",V"w"); ("u",V"v")] in
  test "compose extras"
    (List.length (compose_subst s1 s2) = 4);

  (* Unification failure case *)
  let u1 = Node(("p",2), [|V"x"; V"x"|]) in
  let const_a = Node(("a",0), [||]) in
  let const_b = Node(("b",0), [||]) in
  let u2 = Node(("p",2), [| const_a; const_b |]) in
  test "mgu failure"
    (try let _ = mgu u1 u2 in false with NOT_UNIFIABLE -> true);

  (* Nested edit test *)
  let nest = make_node "r" [make_node "s" [V"c"; V"d"]; V"e"] in
  test "nested edit"
    (string_of_term (edit nest [0;1] (V"f")) = "r(s(c,f),e)");

  (* In-place deeper subst *)
  let ip = make_node "t" [make_node "u" [V"p"]; V"q"] in
  test "inplace deep"
    (string_of_term (inplace_subst ip [("p", V"r")]) = "t(u(r),q)");

  Printf.printf "All tests passed.\n"
