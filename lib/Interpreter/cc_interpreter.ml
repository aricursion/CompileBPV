open Cc_ast

type 'a state = Final of 'a | Stepping of 'a

(* substitutes v1 for x in v2 *)
let rec substVal (v1 : value_term) (x : Variable.t) (v2 : value_term) : value_term =
  (match v2 with
  | Var x' -> if Variable.compare x x' = 0 then v1 else v2
  | TensorProd vs -> TensorProd (List.map (substVal v1 x) vs)
  | Inj (t, i, v) -> Inj (t, i, substVal v1 x v)
  | Pack (typop, t, v) -> Pack (typop, t, substVal v1 x v)
  | Close c -> Close (substComp v1 x c))

(* substitutes v for x in c *)
and substComp (v : value_term) (x : Variable.t) (c : comp_term) : comp_term =
  (match c with
  | Ret v' -> Ret (substVal v x v')
  | Bind (c1, x', c2) -> Bind (substComp v x c1, x', substComp v x c2)
  | CLet (x', v', c') -> CLet (x', substVal v x v', substComp v x c')
  | Lam (x', t, c') -> Lam (x', t, substComp v x c')
  | Ap (c', v') -> Ap (substComp v x c', substVal v x v')
  | Split (v', (xs, c')) -> Split (substVal v x v', (xs, substComp v x c'))
  | Case (v', arms) -> Case (substVal v x v', List.map (fun (x', c') -> (x', substComp v x c')) arms)
  | Print s -> Print s
  | Unpack (v', (x', c')) -> Unpack (substVal v x v', (x', substComp v x c'))
  | Open v' -> Open (substVal v x v'))

let rec progressTensor (vs : value_term list) (acc : value_term list) =
  (match vs with
  | [] -> Final (TensorProd (List.rev acc))
  | v::vs' -> 
    (match progressVal v with
    | Final v' -> progressTensor vs' (v'::acc)
    | Stepping v' -> Stepping (TensorProd (List.rev acc @ (v' :: vs')))))

and progressVal (v : value_term) : value_term state =
  (match v with
  | Var _ -> failwith "you can only step closed expressions"
  | TensorProd vs -> progressTensor vs []
  | Inj (t, i, v) -> 
    (match progressVal v with
    | Final v' -> Final (Inj (t, i, v'))
    | Stepping v' -> Stepping (Inj (t, i, v')))
  | Pack (typop, t, v) -> 
    (match progressVal v with
    | Final v' -> Final (Pack (typop, t, v'))
    | Stepping v' -> Stepping (Pack (typop, t, v')))
  | Close c -> Final (Close c))

let rec progressComp (c : comp_term) : comp_term state =
  (match c with
  | Ret v -> 
    (match progressVal v with
    | Final v' -> Final (Ret v')
    | Stepping v' -> Stepping (Ret v'))
  | Bind (c1, x, c2) -> 
    (match progressComp c1 with
    | Final (Ret v) -> Stepping (substComp v x c2)
    | Final _ -> failwith "Final command for bind somehow not in canonical form for F(A)"
    | Stepping c1' -> Stepping (Bind (c1', x, c2)))
  | CLet (x, v1, c') ->
    (match progressVal v1 with
    | Final v1' -> Stepping (substComp v1' x c')
    | Stepping v1' -> Stepping (CLet (x, v1', c')))
  | Lam (x, t, c') -> Final (Lam (x, t, c'))
  | Ap (c', v) ->
    (match progressComp c' with
    | Final (Lam (x, _, c') as c'') -> 
      (match progressVal v with 
      | Final v' -> Stepping (substComp v' x c')
      | Stepping v' -> Stepping (Ap (c'', v')))
    | Final _ -> failwith "Final command for ap somehow not in canonical form for A -> X"
    | Stepping c'' -> Stepping (Ap (c'', v)))
  | Open v ->
    (match progressVal v with
    | Final (Close c) -> Stepping c
    | Final _ -> failwith "Final value for open somehow not in canonical form for UU(X)"
    | Stepping v' -> Stepping (Open v'))
  | Split (v, (xs, c')) -> 
    (match progressVal v with
    | Final (TensorProd vs) -> Stepping (List.fold_left (fun acc (x, v) -> substComp v x acc) c' (List.combine xs vs))
    | Final _ -> failwith "Final value for split somehow not in canonical form for tensor"
    | Stepping v' -> Stepping (Split (v', (xs, c'))))
  | Case (v, arms) -> 
    (match progressVal v with
    | Final (Inj (_, i, v')) -> let (x, c') = List.nth arms (i - 1) in Stepping (substComp v' x c')
    | Final _ -> failwith "Final value for case somehow not in canonical form for sum"
    | Stepping v' -> Stepping (Case (v', arms)))
  | Print s -> print_endline s; Final (Ret (TensorProd []))
  | Unpack (v, (x, c)) -> 
    (match progressVal v with
    | Final (Pack (_, _, v')) -> Stepping (substComp v' x c)
    | Final _ -> failwith "Final value for unpack somehow not in canonical form for existential type"
    | Stepping v' -> Stepping (Unpack (v', (x, c)))))

let rec interpret (c : comp_term) : value_term =
  match progressComp c with
  | Final (Ret v) -> v
  | Final _ -> failwith "terminated with wrong canonical form"
  | Stepping c' -> interpret c'