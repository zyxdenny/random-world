structure GrammarAst :
sig
  datatype exp_func =
      F0 of string * (unit -> real)
    | F1 of string * (real -> real)
    | F2 of string * (real -> real -> real)
    | F3 of string * (real -> real -> real -> real)
    | F4 of string * (real -> real -> real -> real -> real)

  datatype rule =
      R of exp_func * (rule list)

  val grammar2str : rule -> string
end =
struct
  (* string is the function name; for printing purpose *)
  datatype exp_func =
      F0 of string * (unit -> real)
    | F1 of string * (real -> real)
    | F2 of string * (real -> real -> real)
    | F3 of string * (real -> real -> real -> real)
    | F4 of string * (real -> real -> real -> real -> real)

  datatype rule =
      R of exp_func * (rule list)

  fun map_first_n (f, lst) n =
  let
    fun map_first_n_acc (f, lst) n res =
      if n = 0 then List.rev res
      else
        case lst of
             [] => raise Fail "Too few arguents."
           | x::lst_tail =>
               map_first_n_acc (f, lst_tail) (n - 1) ((f x)::res)
  in
    map_first_n_acc (f, lst) n []
  end

  fun concat_with_comma str_lst =
    case str_lst of
         [] => ""
       | [str] => str
       | str::str_lst_tail => str ^ ", " ^ (concat str_lst_tail)

  fun grammar2str (R (f_str, r_ls)) =
    case f_str of
         F0 (name, f) => name ^ "()"
       | F1 (name, f) =>
           name ^ "(" ^ (concat_with_comma (map_first_n (grammar2str, r_ls) 1)) ^ ")"
       | F2 (name, f) =>
           name ^ "(" ^ (concat_with_comma (map_first_n (grammar2str, r_ls) 2)) ^ ")"
       | F3 (name, f) =>
           name ^ "(" ^ (concat_with_comma (map_first_n (grammar2str, r_ls) 3)) ^ ")"
       | F4 (name, f) =>
           name ^ "(" ^ (concat_with_comma (map_first_n (grammar2str, r_ls) 4)) ^ ")"
end


(*
structure ExpressionTree :>
sig
  type t
end =
struct
  datatype exp_func =
      F0 of () -> real
    | F1 of real -> real
    | F2 of real -> real -> real
    | F3 of real -> real -> real -> real
    | F4 of real -> real -> real -> real -> real

  datatype exp_tree_node =
    Node of {
      parent: exp_tree_node,
      children: exp_tree_node list,
      f: exp_func,
    }
    
  datatype exp_tree =
    Tree of {
      leaves:
    }
end
*)
