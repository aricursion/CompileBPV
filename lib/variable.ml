type t = string

let ctr = ref 0

let new_var _ = ctr := !ctr + 1 ; string_of_int !ctr

let of_string s = s

let compare = String.compare 

let pp_var x = "var_" ^ x