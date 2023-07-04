open Core

let read_data () = In_channel.read_all "data.txt" |> String.strip

let make_move n = function
  | '(' -> n + 1
  | ')' -> n - 1
  | n -> failwithf "unexpected input: %c" n ()
;;

let () =
  let data = read_data () in
  String.to_list data
  |> List.fold ~init:0 ~f:make_move
  |> Int.to_string
  |> printf "floor: %s\n";
  let n = ref 0 in
  String.to_list data
  |> List.findi ~f:(fun _ c ->
       n := make_move !n c;
       !n < 0)
  |> (function
       | Some (i, _) -> i + 1
       | None -> failwith "could not find negative floor")
  |> Int.to_string
  |> printf "negative floor: %s\n"
;;
