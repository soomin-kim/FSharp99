open System

let cfmt_print state etc =
  match state with
  | "[Passed]" -> Console.ForegroundColor <- ConsoleColor.Green
  | "[Failed]" -> Console.ForegroundColor <- ConsoleColor.Red
  Console.Write (state + " ")
  Console.ForegroundColor <- ConsoleColor.White
  printfn etc

let checker1 num prog input expected =
  let output = prog input
  if output = expected then cfmt_print "[Passed]" "Test #%02d" num
  else cfmt_print "[Failed]" "Test #%02d Expected %A But %A" num expected output

let checker2 num prog input1 input2 expected =
  let output = prog input1 input2
  if output = expected then cfmt_print "[Passed]" "Test #%02d" num
  else cfmt_print "[Failed]" "Test #%02d Expected %A But %A" num expected output

let checker3 num prog input1 input2 input3 expected =
  let output = prog input1 input2 input3
  if output = expected then cfmt_print "[Passed]" "Test #%02d" num
  else cfmt_print "[Failed]" "Test #%02d Expected %A But %A" num expected output

let rec list_last_01 lst =
  match lst with
  | []        -> None
  | [elem]    -> Some elem
  | _ :: tail -> list_last_01 tail

checker1 1 list_last_01 [] None
checker1 1 list_last_01 ["a" ; "b" ; "c" ; "d"] (Some "d")

let rec list_last_but_one_02 lst =
  match lst with
  | []              -> None
  | [_]             -> None
  | [elem1 ; elem2] -> Some (elem1 , elem2)
  | _ :: tail       -> list_last_but_one_02 tail

checker1 2 list_last_but_one_02 [] None
checker1 2 list_last_but_one_02 ["a"] None
checker1 2 list_last_but_one_02 ["a" ; "b" ; "c" ; "d"] (Some ("c" , "d"))

let rec list_at_03 idx lst =
  match lst with
  | [] -> None
  | _ :: _ when idx < 1 -> None
  | head :: _ when idx = 1 -> Some head
  | _ :: tail -> list_at_03 (idx - 1) tail

checker2 3 list_at_03 3 [] None
checker2 3 list_at_03 3 ["a"] None
checker2 3 list_at_03 3 ["a" ; "b" ; "c" ; "d" ; "e"] (Some "c")

let list_length_04 lst =
  let rec helper len lst =
    match lst with
    | []        -> len
    | _ :: tail -> helper (len + 1) tail
  in
  helper 0 lst

checker1 4 list_length_04 [] 0
checker1 4 list_length_04 ["a" ; "b" ; "c"] 3

let list_reverse_05 lst =
  let rec helper result lst =
    match lst with
    | [] -> result
    | head :: tail -> helper (head :: result) tail
  in
  helper [] lst

checker1 5 list_reverse_05 [] []
checker1 5 list_reverse_05 ["a" ; "b" ; "c"] ["c" ; "b" ; "a"]
