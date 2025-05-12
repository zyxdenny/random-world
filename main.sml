structure ET = ExpressionTree

val ((t_r, cnt_r), ((t_g, cnt_g), (t_b, cnt_b))) =
  ForkJoin.par (
    fn _ => ET.generate_tree 0 15 (7875, 89),
    fn _ => ForkJoin.par (
      fn _ => ET.generate_tree 0 20 (231, 631),
      fn _ => ET.generate_tree 0 20 (65, 979)
    )
  )

val ((node_aseq_r, edge_aseq_r), ((node_aseq_g, edge_aseq_g), (node_aseq_b, edge_aseq_b))) = 
  ForkJoin.par (
    fn _ => ET.expr2aseq (t_r, cnt_r),
    fn _ => ForkJoin.par (
      fn _ => ET.expr2aseq (t_g, cnt_g),
      fn _ => ET.expr2aseq (t_b, cnt_b)
    )
  )


val height = 50
val width = 50

fun normalize (x: real) (size: real) =
  x / (size / 2.0) - 1.0


val image_data =
  let
    fun f cnt =
      let
        val i = cnt div width
        val j = cnt mod width
        val (v_r, (v_g, v_b)) =
          ForkJoin.par (
            fn _ => ET.evaluate (node_aseq_r, edge_aseq_r)
              (normalize (Real.fromInt i) (Real.fromInt height),
               normalize (Real.fromInt j) (Real.fromInt width)),
            fn _ => ForkJoin.par (
              fn _ => ET.evaluate (node_aseq_g, edge_aseq_g)
                (normalize (Real.fromInt i) (Real.fromInt height),
                 normalize (Real.fromInt j) (Real.fromInt width)),
              fn _ => ET.evaluate (node_aseq_b, edge_aseq_b)
                (normalize (Real.fromInt i) (Real.fromInt height),
                 normalize (Real.fromInt j) (Real.fromInt width))
            )
          )
      in
        { red   = Word8.fromInt (Real.round ((v_r + 1.0) * 255.0)),
          green = Word8.fromInt (Real.round ((v_g + 1.0) * 255.0)),
          blue  = Word8.fromInt (Real.round ((v_b + 1.0) * 255.0)) }
      end
  in
    Seq.tabulate f (width * height)
  end

val () = PPM.write "img.ppx" {height = height, width = width, data = image_data}
(*
val _ =
  let
    val res_seq =
      Benchmark.run (
        fn _ => ET.evaluate_sequential t (normalize 10.0, normalize 11.0))
  in
    print ("The correct result is " ^ (Real.toString res_seq) ^ "\n")
  end

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

