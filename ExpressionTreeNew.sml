structure ExpressionTree = 
struct
  val par = ForkJoin.par
  val alloc = ForkJoin.alloc
  val rand = Random.rand
  val randRange = Random.randRange
  val randReal = Random.randReal
  val randInt = Random.randInt

  structure A = Array
  structure ASeq = ArraySequence 

  datatype expr =
      CONST of real
    | X
    | Y
    (*
    | NEG of expr
    | SIN of expr
    | COS of expr
    | SQRT of expr
    | ABS of expr
    *)
    | ADD of expr * expr
    | MULT of expr * expr
    (*
    | MAX of expr * expr
    | MIN of expr * expr
    *)

  fun raise_bug str =
    raise Fail ("BUG: " ^ str)

  fun generate_tree depth max_depth (seed1, seed2) : expr * int =
    let
      val rng = rand (seed1, seed2)
    in
      if depth > max_depth then
        case randRange (0, 2) rng of
             0 => (X, 1)
           | 1 => (Y, 1)
           | _ => (CONST (randReal rng), 1)
      else
        let
          val rng = rand (seed1, seed2)
          val rng' =
            let
              val seed = randInt rng
            in
              rand (seed, seed + 1)
            end
          val rng'' =
            let
              val seed = randInt rng'
            in
              rand (seed, seed + 1)
            end
          val seed_pair1 = (randInt rng, randInt rng')
          val seed_pair2 = (randInt rng, randInt rng'')
          val exp_next = generate_tree (depth + 1) max_depth
        in
          if depth < 3 then
            case randRange (0, 2) rng of
                 0 =>
                   let
                     val ((a, cnt1), (b, cnt2)) =
                       par (
                         fn _ => exp_next seed_pair1,
                         fn _ => exp_next seed_pair2
                       )
                   in
                     (ADD (a, b), cnt1 + cnt2 + 1)
                   end
               | _ =>
                   let
                     val ((a, cnt1), (b, cnt2)) =
                       par (
                         fn _ => exp_next seed_pair1,
                         fn _ => exp_next seed_pair2
                       )
                   in
                     (MULT (a, b), cnt1 + cnt2 + 1)
                   end
          else
            case randRange (0, 12) rng of
                 0  => (CONST (randReal rng), 1)
               | 1  => (X, 1)
               | 2  => (Y, 1)
               | 3  =>
                   let
                     val ((a, cnt1), (b, cnt2)) =
                       par (
                         fn _ => exp_next seed_pair1,
                         fn _ => exp_next seed_pair2
                       )
                   in
                     (ADD (a, b), cnt1 + cnt2 + 1)
                   end
               | _  =>
                   let
                     val ((a, cnt1), (b, cnt2)) =
                       par (
                         fn _ => exp_next seed_pair1,
                         fn _ => exp_next seed_pair2
                       )
                   in
                     (MULT (a, b), cnt1 + cnt2 + 1)
                   end
        end
    end

  (* Sequential way to generate a tree; not recommended *)
  fun generate_tree_sequential depth max_depth (seed1, seed2) =
    let
      val rng = rand (seed1, seed2)
    in
      if depth > max_depth then
        case randRange (0, 2) rng of
             0 => X
           | 1 => Y
           | _ => CONST (randReal rng)
      else
        let
          val rng = rand (seed1, seed2)
          val rng' =
            let
              val seed = randInt rng
            in
              rand (seed, seed + 1)
            end
          val rng'' =
            let
              val seed = randInt rng'
            in
              rand (seed, seed + 1)
            end
          val seed_pair1 = (randInt rng, randInt rng')
          val seed_pair2 = (randInt rng, randInt rng'')
          val exp_next = generate_tree_sequential (depth + 1) max_depth
        in
          if depth < 3 then
            case randRange (0, 1) rng of
                 0 => ADD  (exp_next seed_pair1, exp_next seed_pair2)
               | _ => MULT (exp_next seed_pair1, exp_next seed_pair2)
          else
            case randRange (0, 4) rng of
                 0  => CONST (randReal rng)
               | 1  => X
               | 2  => Y
               | 3  => ADD   (exp_next seed_pair1, exp_next seed_pair2)
               | _  => MULT  (exp_next seed_pair1, exp_next seed_pair2)
        end
    end


  fun tree2str (tree: expr) =
    case tree of
         CONST c => Real.toString c
       | X => "x"
       | Y => "y"
       (*
       | NEG e => "-(" ^ (tree2str e) ^ ")"
       | SIN e => "sin(" ^ (tree2str e) ^ ")"
       | COS e => "cos(" ^ (tree2str e) ^ ")"
       | SQRT e => "sqrt(" ^ (tree2str e) ^ ")"
       | ABS e => "|" ^ (tree2str e) ^ "|"
       *)
       | ADD (e1, e2) =>
           let 
             val (str1, str2) =
               par (
                 fn _ => tree2str e1,
                 fn _ => tree2str e2
               )
           in
             "(" ^ str1 ^ ") + (" ^ str2 ^ ")"
           end
       | MULT (e1, e2) =>
           let 
             val (str1, str2) =
               par (
                 fn _ => tree2str e1,
                 fn _ => tree2str e2
               )
           in
             "(" ^ str1 ^ ") * (" ^ str2 ^ ")"
           end
       (*
       | MAX (e1, e2) => "max(" ^ (tree2str e1) ^ ", " ^ (tree2str e2) ^ ")"
       | MIN (e1, e2) => "min(" ^ (tree2str e1) ^ ", " ^ (tree2str e2) ^ ")"
       *)

  fun evaluate_sequential (tree: expr) (x, y) =
    case tree of
         CONST c => c
       | X => x
       | Y => y
       (*
       | NEG e => ~(evaluate_sequential e (x, y))
       | SIN e => Math.sin (Math.pi * (evaluate_sequential e (x, y)))
       | COS e => Math.cos (Math.pi * (evaluate_sequential e (x, y)))
       | SQRT e => (Math.sqrt ((1.0 + (evaluate_sequential e (x, y))) * 2.0)) - 1.0
       | ABS e => abs (evaluate_sequential e (x, y))
       *)
       | ADD (e1, e2) => ((evaluate_sequential e1 (x, y)) + (evaluate_sequential e2 (x, y))) / 2.0
       | MULT (e1, e2) => (evaluate_sequential e1 (x, y)) * (evaluate_sequential e2 (x, y))
       (*
       | MAX (e1, e2) => max (evaluate_sequential e1 (x, y)) (evaluate_sequential e2 (x, y))
       | MIN (e1, e2) => min (evaluate_sequential e1 (x, y)) (evaluate_sequential e2 (x, y))
       *)

  fun evaluate_tree_par_naive (tree: expr) (x, y) =
    case tree of
         CONST c => c
       | X => x
       | Y => y
       (*
       | NEG e => ~(evaluate_tree_par_naive e (x, y))
       | SIN e => Math.sin (Math.pi * (evaluate_tree_par_naive e (x, y)))
       | COS e => Math.cos (Math.pi * (evaluate_tree_par_naive e (x, y)))
       | SQRT e => (Math.sqrt ((1.0 + (evaluate_tree_par_naive e (x, y))) * 2.0)) - 1.0
       | ABS e => abs (evaluate_tree_par_naive e (x, y))
       *)
       | ADD (e1, e2) =>
           let
             val (a, b) =
               par (
                 fn _ => evaluate_tree_par_naive e1 (x, y),
                 fn _ => evaluate_tree_par_naive e2 (x, y)
               )
           in
             (a + b) / 2.0
           end
       | MULT (e1, e2) =>
           let
             val (a, b) =
               par (
                 fn _ => evaluate_tree_par_naive e1 (x, y),
                 fn _ => evaluate_tree_par_naive e2 (x, y)
               )
           in
             a * b
           end
       (*
       | MAX (e1, e2) =>
           let
             val (a, b) =
               ForkJoin.par (
                 fn _ => evaluate_tree_par_naive e1 (x, y),
                 fn _ => evaluate_tree_par_naive e2 (x, y)
               )
           in
             max a b
           end
       | MIN (e1, e2) =>
           let
             val (a, b) =
               ForkJoin.par (
                 fn _ => evaluate_tree_par_naive e1 (x, y),
                 fn _ => evaluate_tree_par_naive e2 (x, y)
               )
           in
             min a b
           end
       *)

  (* Rake and Compress *)
  datatype func =
      ADD_F
    | MULT_F

  datatype leaf_t =
      X_T
    | Y_T
    | CONST_T of real

  datatype rnode =
      UNODE of real * real
    | BNODE of func
    | LEAF of leaf_t

  type node = int * rnode

  datatype edge =
      E1 of int * int           (* parent has one child *)
    | E2 of int * (int * int)   (* parent has two children *)


  fun get_leaf_val (x, y) l =
    case l of
         CONST_T const => const
       | X_T => x
       | Y_T => y


  (* cnt is the number of nodes in the expression tree *)
  fun expr2aseq (t, cnt) : (node ASeq.t) * (edge ASeq.t) =
    let
      (* val arr = Array.array (cnt, LEAF 0.0) *)
      val node_arr = alloc cnt

      fun build e index edge_lst =
        case e of
             CONST c => 
               let
                 val _ = A.update (node_arr, index, (index, LEAF (CONST_T c)))
               in
                 (index, edge_lst)
               end
           | X => 
               let
                 val _ = A.update (node_arr, index, (index, LEAF X_T))
               in
                 (index, edge_lst)
               end
           | Y => 
               let
                 val _ = A.update (node_arr, index, (index, LEAF Y_T))
               in
                 (index, edge_lst)
               end
           | ADD (e1, e2) =>
               let
                 val left_index = index + 1
                 val (tmp_index, edge_lst') = build e1 left_index edge_lst
                 val right_index = tmp_index + 1
                 val (final_index, edge_lst'') = build e2 right_index edge_lst'
                 val _ = A.update (node_arr, index, (index, BNODE ADD_F))
               in
                 let
                   val edge_lst''' = (E2 (index, (left_index, right_index)))
                                     :: edge_lst''
                 in
                   (final_index, edge_lst''')
                 end
               end
           | MULT (e1, e2) =>
               let
                 val left_index = index + 1
                 val (tmp_index, edge_lst') = build e1 left_index edge_lst
                 val right_index = tmp_index + 1
                 val (final_index, edge_lst'') = build e2 right_index edge_lst'
                 val _ = A.update (node_arr, index, (index, BNODE MULT_F))
               in
                 let
                   val edge_lst''' = (E2 (index, (left_index, right_index)))
                                     :: edge_lst''
                 in
                   (final_index, edge_lst''')
                 end
               end

      val (_, edge_lst) = build t 0 []
    in
      (ASeq.fromArray node_arr, ASeq.fromList edge_lst) 
    end


  (* evaluate expression tree by rake-and-compress *)
  fun is_leaf_edge node_aseq e =
    case e of
         E1 (_, c) =>
           let
             val (_, rn) = ASeq.nth node_aseq c
           in
             case rn of
                  LEAF _ => true
                | _ => false
           end
       | E2 (_, (c1, c2)) =>
           let
             val (_, rn1) = ASeq.nth node_aseq c1
             val (_, rn2) = ASeq.nth node_aseq c2
           in
             case (rn1, rn2) of
                  (LEAF _, _) => true
                | (_, LEAF _) => true
                | _ => false
           end


  fun map_to_inject_node (x, y) node_aseq e =
    case e of
         E1 (p, c) =>
           let
             val (_, rpn) = ASeq.nth node_aseq p
             val (_, rcn) = ASeq.nth node_aseq c
             val (a1, a0) = case rpn of UNODE co => co
                                      | _ => raise_bug "Has to be unode."
             val l = case rcn of LEAF l => l
                               | _ => raise_bug "Has to be leaf."
           in
             (p, (p, LEAF (CONST_T (get_leaf_val (x, y) l))))
           end
       | E2 (p, (c1, c2)) =>
           let
             val (_, rpn) = ASeq.nth node_aseq p
             val (_, rcn1) = ASeq.nth node_aseq c1
             val (_, rcn2) = ASeq.nth node_aseq c2
             val f = case rpn of BNODE f => f
                               | _ => raise_bug "Has to be bnode"
           in
             case (rcn1, rcn2) of
                  (LEAF l1, LEAF l2) =>
                    let
                      val res =
                        case f of
                             ADD_F =>
                               ((get_leaf_val (x, y) l1) + (get_leaf_val (x, y) l2)) / 2.0
                           | MULT_F =>
                               (get_leaf_val (x, y) l1) * (get_leaf_val (x, y) l2)

                    in
                      (p, (p, LEAF (CONST_T res)))
                    end
                | (LEAF l, _) =>
                    let
                      val (a1, a0) =
                        case f of
                             ADD_F => (1.0 / 2.0, (get_leaf_val (x, y) l) / 2.0)
                           | MULT_F => (get_leaf_val (x, y) l, 0.0)
                    in
                      (p, (p, UNODE (a1, a0)))
                    end
                | (_, LEAF l) =>
                    let
                      val (a1, a0) =
                        case f of
                             ADD_F => (1.0 / 2.0, (get_leaf_val (x, y) l) / 2.0)
                           | MULT_F => (get_leaf_val (x, y) l, 0.0)
                    in
                      (p, (p, UNODE (a1, a0)))
                    end
                | _ => raise_bug "At least one of the edges has to be leaf-edge."
           end


  fun is_internal_edge node_aseq e =
    case e of
         E1 (p, c) =>
           let
             val (_, rpn) = ASeq.nth node_aseq p
             val (_, rcn) = ASeq.nth node_aseq c
           in
             case (rpn, rcn) of
                  (LEAF _, _) => false
                | _ => true
           end
       | E2 (p, (c1, c2)) =>
           let
             val (_, rpn) = ASeq.nth node_aseq p
             val (_, rcn1) = ASeq.nth node_aseq c1
             val (_, rcn2) = ASeq.nth node_aseq c2
           in
             case (rpn, rcn1, rcn2) of
                  (LEAF _, _, _) => false
                | _ => true
           end


  fun map_e2_to_e1 node_aseq e =
    case e of
         E2 (p, (c1, c2)) =>
           let
             val (_, rpn) = ASeq.nth node_aseq p
             val (_, rcn1) = ASeq.nth node_aseq c1
             val (_, rcn2) = ASeq.nth node_aseq c2
           in
             case (rpn, rcn1, rcn2) of
                  (UNODE _, LEAF _, _) => E1 (p, c2)
                | (UNODE _, _, LEAF _) => E1 (p, c1)
                | _ => e
           end
       | _ => e


  fun rake (x, y) (node_aseq, edge_aseq) =
    let
      val to_be_injected =
        let
          val p = is_leaf_edge node_aseq
          val f = map_to_inject_node (x, y) node_aseq
        in
          ASeq.filtermap p f edge_aseq
        end
      val node_aseq' = ASeq.inject (node_aseq, to_be_injected)
      val edge_aseq' =
        let
          val p = is_internal_edge node_aseq'
          val f = map_e2_to_e1 node_aseq'
        in
          ASeq.filtermap p f edge_aseq
        end
    in
      (node_aseq', edge_aseq')
    end


  fun evaluate (node_aseq, edge_aseq) (x, y) =
    let
      fun loop (node_aseq, edge_aseq) (x, y) =
        if ASeq.length edge_aseq = 0 then
          case ASeq.nth node_aseq 0 of
               (_, LEAF l) => get_leaf_val (x, y) l
             | _ => raise_bug "Edges shrink to 0 before nodes shrink to 0."
        else
          let
            val (node_aseq', edge_aseq') = rake (x, y) (node_aseq, edge_aseq)
          in
            loop (node_aseq', edge_aseq') (x, y)
          end
    in
      loop (node_aseq, edge_aseq) (x, y)
    end
end

