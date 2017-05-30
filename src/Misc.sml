structure Misc =
struct

infix contains
fun [] contains  _ = false
  | (x :: xs) contains y = if x = y then true else xs contains y

infix withoutAll
fun xs withoutAll ys =
  List.filter (fn x => not (List.exists (fn y => x = y) ys))  xs

fun lookup [] _ = NONE
  | lookup ((key, value) :: xs) x =
    if key = x then SOME value else lookup xs x

end
