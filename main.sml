structure GA = GrammarAst

val rng = Random.rand (1, 2)

fun randomElement lst =
let
  val length = List.length lst
  val index = Random.randRange (0, length - 1) rng
in
  List.nth (lst, index)
end

fun activate (f, lst) depth =
  (f, List.map (fn x => x depth) lst)


val plus = ("PLUS", GA.F2 (fn (x: real) => fn (y: real) => x + y))
val minus = ("MINUS", GA.F2 (fn (x: real) => fn y => x - y))
val mult = ("MULT", GA.F2 (fn (x: real) => fn y => x * y))
val divi = ("DIV", GA.F2 (fn x => fn y => x / y))
val const = ("CONST", GA.F0 (fn _ => 1.0))


fun generate depth rule_lst =
  if depth > 5 then
    GA.R (const, [])
  else
    GA.R (activate (randomElement rule_lst) (depth + 1))


(* Grammar definition *)
fun genA depth =
  generate depth
  [ (plus, [genA, genA])
  , (minus, [genA, genA])
  (*, (const, [])*)
  ]

and genB depth =
  generate depth
  [ (const, []) ]
  

val () = print ((GA.rule2str (genA 0)) ^ "\n")
val () = print ((Real.toString (GA.evalRuleSequential (genA 0))) ^ "\n")
