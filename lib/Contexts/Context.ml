type 'a t = {
  ctx: (Variable.t, 'a) Hashtbl.t 
}

let empty () = {ctx = Hashtbl.create 0}

let insert ctx x t = Hashtbl.add ctx.ctx x t

let remove ctx x = 
  match Hashtbl.find_opt ctx.ctx x with
  | Some(t) -> Hashtbl.remove ctx.ctx x ; Some(t) 
  | None -> None

let lookup ctx x = Hashtbl.find_opt ctx.ctx x