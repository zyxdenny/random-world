structure ET = ExpressionTree

fun normalize (x: real) =
  x / 500.0 - 1.0

val (t, cnt) = ET.generate_tree 0 15 (100, 20)

val (node_aseq, edge_aseq) = ET.expr2aseq (t, cnt)

val () = print ((ET.tree2str t) ^ "\n")

(*val () = print ("The number of nodes is " ^ (Int.toString cnt) ^ "\n")*)
val _ =
  let
    val res_seq =
      Benchmark.run (
        fn _ => ET.evaluate_sequential t (normalize 10.0, normalize 11.0))
  in
    print ("The correct result is " ^ (Real.toString res_seq) ^ "\n")
  end

(*
val _ =
  let
    val res_rake_compress =
      Benchmark.run (
        fn _ => ET.evaluate (node_aseq, edge_aseq) (normalize 10.0, normalize 11.0))
  in
    print ("The result is " ^ (Real.toString res_rake_compress) ^ "\n")
  end
*)

(*
val _ =
  let
    val res_par_naive =
      Benchmark.run (
        fn _ => ET.evaluate_par_naive t (normalize 10.0, normalize 11.0))
  in
    print ("The result is " ^ (Real.toString res_par_naive) ^ "\n")
  end
*)

(*
val res = ET.evaluate (node_aseq, edge_aseq) (normalize 10.0, normalize 11.0)
val () = print ("The result is " ^ (Real.toString res) ^ "\n")
*)

(*
val res_naive = ET.evaluate_sequential t (normalize 987.0, normalize 676.0) 
val () = print ("The naive parallel result is " ^ (Real.toString res_naive) ^ "\n")
*)

