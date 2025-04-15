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
  val evalRuleParallelNaive : rule -> real
  type node = {parent: int, children: int list, f: exp_func}
  (*val rule2adjlist : rule -> node Array.array*)
  (*val print_adjlist : node Array.array -> unit*)
  val rule2seq : rule -> node Seq.t
  val evalRuleParallel : node Seq.t -> real
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


  fun evalRuleParallelNaive r =
  let
    val (R ((_, f_exp), r_lst)) = r
  in
    case f_exp of
         F0 f => f ()
       | F1 f => f (evalRuleParallelNaive (List.nth (r_lst, 0)))
       | F2 f =>
           let
             val (a, b) =
               ForkJoin.par (
                 fn _ => evalRuleParallelNaive (List.nth (r_lst, 0)),
                 fn _ => evalRuleParallelNaive (List.nth (r_lst, 1))
               )
           in
             f a b
           end
       | F3 f =>
           let
             val ((a, b), c) =
               ForkJoin.par (
                 fn _ => ForkJoin.par (
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 0)),
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 1))
                 ),
                 fn _ => evalRuleParallelNaive (List.nth (r_lst, 2))
               )
           in
             f a b c
           end
       | F4 f =>
           let
             val ((a, b), (c, d)) =
               ForkJoin.par (
                 fn _ => ForkJoin.par (
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 0)),
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 1))
                 ),
                 fn _ => ForkJoin.par (
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 2)),
                   fn _ => evalRuleParallelNaive (List.nth (r_lst, 3))
                 )
               )
           in
             f a b c d
           end
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
          {parent = parent_pos, children = List.rev (!children), f = f_exp})
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

  fun rule2seq r =
  let
    val arr = rule2adjlist r
  in
    Seq.tabulate (fn i => Array.sub (arr, i)) (Array.length arr)
  end


  fun whose_parent this leaves =
    if Seq.length leaves = 0 then
      raise Fail "BUG: Leaf num should not be 0."
    else
      let
        val leaf = Seq.filter (
          fn x =>
            case x of
                 (_, NONE) => raise Fail "BUG: Leaves should not be NONE."
               | (_, SOME data) =>
                   let
                     val {parent = parent, children = _, f = _} = data
                   in
                     parent = this
                   end) leaves
      in
        if Seq.length leaf = 0 then NONE else SOME (Seq.nth leaf 0)
      end


  fun is_leaf this leaves =
    (Seq.length (Seq.filter (
      fn x =>
        case x of
             (_, NONE) => raise Fail "BUG: Leaves should not be NONE."
           | (i, SOME _) =>
               i = this
    ) leaves)) > 0


  fun rake nodes leaves =
    Seq.map (
      fn x =>
        case x of
             (_, NONE) => x
           | (i, SOME data) =>
               let
                 val {parent = parent, children = children, f = f} = data
               in
                 if is_leaf i leaves then (i, NONE)
                 else
                   case whose_parent i leaves of
                        NONE => x
                      | SOME leaf =>
                          case leaf of
                               (_, NONE) => raise Fail "BUG: Leaf should not be NONE."
                             | (_, SOME data) =>
                                 let
                                   val {parent = _, children = _, f = f_child} = data
                                   val value =
                                     case f_child of
                                          F0 f' => f' ()
                                        | _ =>
                                            raise Fail "BUG: A leaf could not have more than 0 arguments"
                                 in
                                   (i,
                                   SOME {parent = parent, children = List.tl children,
                                   f =
                                     case f of
                                          F0 f' =>
                                            raise Fail "BUG: A parent must have at least 1 child; 0 found."
                                        | F1 f' =>
                                            F0 (fn () => f' value)
                                        | F2 f' =>
                                            F1 (f' value)
                                        | F3 f' =>
                                            F2 (f' value)
                                        | F4 f' =>
                                            F3 (f' value)
                                   })
                                 end
               end
    ) nodes


  fun evalRuleParallel node_seq =
  let
    fun loop nodes =
    let
      val raked_leaves = Seq.filter (
        fn x =>
          case x of
               (_, NONE) => false
             | (i, SOME data) => 
                 let
                   val {parent = parent, children = children, f = _} = data
                 in
                   if not (List.null children) then false
                   else
                     if parent = ~1 then false
                     else
                       case Seq.nth nodes parent of
                            (_, NONE) => raise Fail "BUG: No parent found."
                          | (_, SOME data) =>
                              let
                                val {parent = _, children = children', f = _} =
                                  data
                              in
                                (List.hd children' = i)
                              end
                 end) nodes
    in
      if Seq.length raked_leaves = 0 then
        case Seq.nth nodes 0 of
             (0, SOME data) =>
               let
                 val {parent = _, children = _, f = f} = data
               in
                 case f of
                      F0 f' => f' ()
                    | _ => raise Fail "BUG: A leaf function could not have more than 0 arguments"
               end
           | _ => raise Fail "BUG: Parent is NONE?"
      else
        loop (rake nodes raked_leaves)
    end
  in
    loop (Seq.tabulate (fn i => (i, SOME (Seq.nth node_seq i))) (Seq.length node_seq))
  end

end
