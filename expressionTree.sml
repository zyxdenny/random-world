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
  type node = {parent: int, children: int list, f: exp_func}
  val rule2adjlist : rule -> node Array.array
  val print_adjlist : node Array.array -> unit
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


  fun rule2str r =
  let
    val (R ((name, _), r_lst)) = r
    fun concat_with_comma str_lst =
      case str_lst of
           [] => ""
         | [str] => str
         | str::str_lst_tail => str ^ ", " ^ (concat str_lst_tail)
  in
    name ^ "(" ^ (concat_with_comma (List.map rule2str r_lst)) ^ ")"
  end


  fun evalRuleSequential r =
  let
    val (R ((_, f_exp), r_lst)) = r
    val args = List.map (fn x => evalRuleSequential x) r_lst
  in
    case f_exp of
         F0 f => f ()
       | F1 f => f (List.nth (args, 0))
       | F2 f => f (List.nth (args, 0)) (List.nth (args, 1))
       | F3 f => f (List.nth (args, 0)) (List.nth (args, 1)) (List.nth (args, 2))
       | F4 f => f (List.nth (args, 0)) (List.nth (args, 1)) (List.nth (args, 2)) (List.nth (args, 3))
  end

  
  type node = {parent: int, children: int list, f: exp_func}

  fun rule2adjlist r =
  let
    val (R ((_, f_exp), r_lst)) = r
    fun count_nodes (R ((_, f_exp), r_lst)) =
      (List.foldl (op +) 0 (List.map count_nodes r_lst)) + 1
    val num_nodes = count_nodes r
    val arr = ForkJoin.alloc num_nodes
    val pos = ref 0
    fun rule2adjlist_help (R ((_, f_exp), r_lst)) parent_pos =
      let
        val this_pos = !pos
        val children = ref []
        val _ = List.map (fn x =>
          (pos := !pos + 1; children := (!pos)::(!children); rule2adjlist_help x this_pos))
          r_lst
      in
        Array.update (arr, this_pos,
          {parent = parent_pos, children = !children, f = f_exp})
      end
  in
    (rule2adjlist_help r ~1; arr)
  end

  fun print_adjlist arr =
  let
    fun loop arr num =
      if num < (Array.length arr) then
        let
          val {parent = parent, children = children, f = _} = Array.sub (arr, num)
        in
          (print ("(parent: " ^ (Int.toString parent) ^ ", children: [");
          List.map (fn x => print ((Int.toString x) ^ ",")) children;
          print "])\n";
          loop arr (num + 1))
        end
      else
        ()
  in
    loop arr 0
  end

end
