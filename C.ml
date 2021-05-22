let rec read_coins i acc =
  if i == 0 then acc else
    read_coins ( i - 1 ) ( ( Scanf.scanf " %d" ( fun x -> x ) ) :: acc )




let rec gready coins left steps =
if left == 0 then steps else
  match coins with
  | x :: xs ->
    if x > left then gready xs left steps
    else
      gready xs ( left - ( left / x * x  ) ) ( steps + left / x )
  | _ -> 100000




let rec dynamic coins left steps ceiling =
  if steps >= ceiling then 100000 else
  if left == 0 then steps else
    match coins with
    | x :: xs ->
      if x > left then dynamic xs left steps ceiling else
      if left mod x == 0 then steps + left / x else
        let this = dynamic xs ( left - ( left / x * x ) )( steps + left / x ) ceiling in
        let those = dynamic xs left steps ceiling in
        if this < those then this else those
    | _ -> 100000




let rec compare coins i max =
  if i > max then -1 else
  let a = gready coins i 0 in
  let b = dynamic coins i 0 a in
  if a <> b then i else compare coins ( i + 1 ) max




let m coins =
  match coins with
  | x :: xs -> x * 3
  | _ -> 1000000




let print coins =
  let x = compare coins 1 10000 in
  if x == -1 then print_endline "YES"
  else ( print_int x; print_endline "" )




let _ =
let n = Scanf.scanf " %d" ( fun x -> x ) in
let coins = read_coins n [] in




print coins