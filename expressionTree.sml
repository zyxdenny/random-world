structure GrammarAst :>
sig
  datatype exp_func =
      F0 of (unit -> real)
    | F1 of (real -> real)
    | F2 of (real -> real -> real)
    | F3 of (real -> real -> real -> real)
    | F4 of (real -> real -> real -> real -> real)

  type exp_func_name = string * exp_func

  datatype rule =
      R of exp_func_name * (rule list)

  val rule2str : rule -> string
  val evalRuleSequential : rule -> real
end =
struct
  (* string is the function name; for printing purpose *)
  datatype exp_func =
      F0 of (unit -> real)
    | F1 of (real -> real)
    | F2 of (real -> real -> real)
    | F3 of (real -> real -> real -> real)
    | F4 of (real -> real -> real -> real -> real)

  type exp_func_name = string * exp_func

  datatype rule =
      R of exp_func_name * (rule list)


  fun rule2str (R ((name, _), r_lst)) =
  let
    fun concat_with_comma str_lst =
      case str_lst of
           [] => ""
         | [str] => str
         | str::str_lst_tail => str ^ ", " ^ (concat str_lst_tail)
  in
    name ^ "(" ^ (concat_with_comma (List.map rule2str r_lst)) ^ ")"
  end


  fun evalRuleSequential (R ((_, f_exp), r_lst)) =
  let
    val args = List.map (fn x => evalRuleSequential x) r_lst
  in
    case f_exp of
         F0 f => f ()
       | F1 f => f (List.nth (args, 0))
       | F2 f => f (List.nth (args, 0)) (List.nth (args, 1))
       | F3 f => f (List.nth (args, 0)) (List.nth (args, 1)) (List.nth (args, 2))
       | F4 f => f (List.nth (args, 0)) (List.nth (args, 1)) (List.nth (args, 2)) (List.nth (args, 3))
  end
end
