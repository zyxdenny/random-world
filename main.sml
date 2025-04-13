open MLton
structure GA = GrammarAst

val r = ref 0

fun randomInt (min, max) =
let
  val range = max - min + 1
  val randomWord = Random.rand()
  (* val randomInt = (Word.toInt randomWord) mod range + min *)
  val randomInt = (r := !r + 1; (!r - 1) mod range + min)
in
  randomInt
end


fun randomElement lst =
let
  val length = List.length lst
  val index = randomInt (0, length - 1)
in
  List.nth(lst, index)
end

fun fst (a, _) = a
fun snd (_, b) = b

fun activate (f, lst) depth =
  (f, List.map (fn x => x depth) lst)


val plus = GA.F2 ("PLUS", fn x => fn y => x + y)
val minus = GA.F2 ("MINUS", fn x => fn y => x - y)
val mult = GA.F2 ("MULT", fn x => fn y => x * y)
val divi = GA.F2 ("DIV", fn x => fn y => x / y)
val const = GA.F0 ("const", fn _ => 0.0)


fun generate depth rule_lst =
  if depth > 5 then
    GA.R (const, [])
  else
    GA.R (activate (randomElement rule_lst) (depth + 1))


fun genA depth =
  generate depth
  [ (plus, [genA, genA])
  , (minus, [genA, genA])
  (*, (const, [])*)
  ]

and genB depth =
  generate depth
  [ (const, []) ]
  

val () = print (GA.grammar2str (genA 0))
