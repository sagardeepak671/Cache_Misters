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

let test_case description f =
  try
    f (); 
    Printf.printf "[ OK ] %s\n" description
  with
  | Assert_failure _ -> Printf.printf "[FAIL] %s\n" description
  | WRONG_SIGNATURE msg -> Printf.printf "[ERROR] %s: WRONG_SIGNATURE(%s)\n" description msg
  | NOT_UNIFIABLE -> Printf.printf "[ERROR] %s: NOT_UNIFIABLE\n" description

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
    let t_invalid1 = Node(("f", 2), [| V "x" |]) in
    assert (not (wfterm sig_valid t_invalid1))
  )
let () = test_case "wfterm t_invalid2" (fun () ->
    let t_invalid2 = Node(("unknown", 1), [| V "x" |]) in
    assert (not (wfterm sig_valid t_invalid2))
  )

(* ---------- TERM PROPERTIES TESTS ---------- *)
let t_complex = Node(("f", 2), [|
    Node(("g", 1), [| Node(("g", 1), [| V "x" |]) |]);
    Node(("g", 1), [| Node(("g", 1), [| Node(("g", 1), [| V "y" |]) |]) |])
  |])

let s1 = [("x", Node(("h", 0), [||]))]
let s2 = [("y", Node(("h", 0), [||]))]
let t_subbed = subst t_complex s1
let comp = compose_subst s1 s2

let () = test_case "ht t_complex" (fun () ->
    assert (ht t_complex = 4)
  )
let () = test_case "size t_complex" (fun () ->
    assert (size t_complex = 8)
  )
let () = test_case "vars t_complex" (fun () ->
    let vs = List.sort compare (vars t_complex) in
    assert (vs = ["x"; "y"])
  )

(* ---------- SUBSTITUTION AND COMPOSITION TESTS ---------- *)
let () = test_case "subst s1 size" (fun () ->
    assert (size t_subbed = 8)
  )

let () = test_case "compose_subst on x" (fun () ->
    match subst (V "x") comp with
    | Node(("h", 0), _) -> ()
    | _ -> assert false
  )
let () = test_case "compose_subst on y" (fun () ->
    match subst (V "y") comp with
    | Node(("h", 0), _) -> ()
    | _ -> assert false
  )
let () = test_case "compose_subst on z" (fun () ->
    match subst (V "z") comp with
    | V "z" -> ()
    | _ -> assert false
  )

(* ---------- MGU TESTS ---------- *)
let () = test_case "mgu trivial unification" (fun () ->
    let t1 = V "x" and t2 = V "x" in
    let u = mgu t1 t2 in
    assert (subst t1 u = subst t2 u)
  )
let () = test_case "mgu unify variable with node" (fun () ->
    let t1 = V "x" in
    let t2 = Node(("h", 0), [||]) in
    let u = mgu t1 t2 in
    assert (subst t1 u = subst t2 u)
  )
let () = test_case "mgu failure occurs check" (fun () ->
    let t1 = V "x" in
    let t2 = Node(("g", 1), [| V "x" |]) in
    (try let _ = mgu t1 t2 in assert false with NOT_UNIFIABLE -> ())
  )
let () = test_case "mgu non-trivial" (fun () ->
    let t1 = Node(("f", 2), [| V "x"; Node(("h", 0), [||]) |]) in
    let t2 = Node(("f", 2), [| Node(("g", 1), [| V "z" |]); V "y" |]) in
    let u = mgu t1 t2 in
    assert (subst t1 u = subst t2 u)
  )
let () = test_case "mgu failure symbol mismatch" (fun () ->
    let t1 = Node(("f", 2), [| V "x"; V "y" |]) in
    let t2 = Node(("g", 2), [| V "x"; V "y" |]) in
    (try let _ = mgu t1 t2 in assert false with NOT_UNIFIABLE -> ())
  )

(* ---------- EDIT TESTS ---------- *)
let () = test_case "edit test" (fun () ->
    let t_editable = Node(("f", 2), [| V "x"; V "y" |]) in
    let edited = edit t_editable [0] (Node(("h", 0), [||])) in
    match edited with
    | Node(("f", 2), [| Node(("h", 0), _); V "y" |]) -> ()
    | _ -> assert false
  )

(* ---------- IN-PLACE SUBSTITUTION TESTS ---------- *)
let () = test_case "inplace_subst test" (fun () ->
    let t_in = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
    let subs = [("x", Node(("h", 0), [||])); ("y", Node(("h", 0), [||]))] in
    ignore (inplace_subst t_in subs);
    match t_in with
    | Node(("f", 2), [| Node(("h", 0), _); Node(("g", 1), [| Node(("h", 0), _) |]) |]) -> ()
    | _ -> assert false
  )

let () = Printf.printf "New test suite executed.\n"
