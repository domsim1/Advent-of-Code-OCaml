open Core

let has_vowel = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> 1
  | _ -> 0
;;

let has_double a b = if Char.equal a b then 1 else 0

let has_bad_pair a b =
  let s = String.of_char_list [ a; b ] in
  match s with
  | "ab" | "cd" | "pq" | "xy" -> 1
  | _ -> 0
;;

let has_repeating a b c = if Char.( = ) a c && Char.( <> ) a b then 1 else 0
let is_pair_safe a b c = (Char.( = ) a b && Char.( <> ) a c) || Char.( <> ) a b

let is_nice s =
  let rec loop vowels doubles bads = function
    | [] -> vowels > 2 && doubles > 0 && bads = 0
    | x :: [] -> loop (vowels + has_vowel x) doubles bads []
    | x :: x' :: xs ->
      loop
        (vowels + has_vowel x)
        (doubles + has_double x x')
        (bads + has_bad_pair x x')
        (x' :: xs)
  in
  loop 0 0 0 @@ String.to_list s
;;

let is_nice_two s =
  let doubles_table = Hashtbl.create (module String) in
  let rec loop repeats = function
    | [] -> Hashtbl.count doubles_table ~f:(fun b -> b > 1) > 0 && repeats > 0
    | _ :: [] -> loop repeats []
    | x1 :: x2 :: x3 :: xs ->
      if is_pair_safe x1 x2 x3
      then Hashtbl.incr doubles_table (String.of_char_list [ x1; x2 ]);
      loop (repeats + has_repeating x1 x2 x3) (x2 :: x3 :: xs)
    | x1 :: x2 :: xs ->
      Hashtbl.incr doubles_table (String.of_char_list [ x1; x2 ]);
      loop repeats (x2 :: xs)
  in
  loop 0 @@ String.to_list s
;;

let count_nice_strings ss = List.count ss ~f:is_nice
let count_nice_strings_two ss = List.count ss ~f:is_nice_two

let () =
  let data = In_channel.read_all "data.txt" |> String.strip |> String.split ~on:'\n' in
  count_nice_strings data |> Int.to_string |> print_endline;
  count_nice_strings_two data |> Int.to_string |> print_endline
;;
