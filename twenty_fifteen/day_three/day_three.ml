open Core

type pos =
  { x : int
  ; y : int
  }

let read_data () = In_channel.read_all "data.txt" |> String.strip |> String.to_list
let new_pos x y = { x; y }
let get_key p = String.concat [ Int.to_string p.x; Int.to_string p.y ]

let move_pos p = function
  | '>' -> new_pos (p.x + 1) p.y
  | '<' -> new_pos (p.x - 1) p.y
  | '^' -> new_pos p.x (p.y + 1)
  | 'v' -> new_pos p.x (p.y - 1)
  | c -> failwithf "could not match %c with direction" c ()
;;

let deliver_flyer house_grid key =
  match Hashtbl.add house_grid ~key ~data:1 with
  | `Ok -> 1
  | `Duplicate ->
    Hashtbl.incr house_grid key;
    0
;;

let () =
  let house_grid = Hashtbl.create (module String) in
  let rec iter total_houses_with_flyers bob_pos robo_pos directions =
    let bob_key = get_key bob_pos in
    let robo_key = get_key robo_pos in
    let inc =
      deliver_flyer house_grid bob_key
      + deliver_flyer house_grid robo_key
      + total_houses_with_flyers
    in
    match directions with
    | [] -> inc
    | x :: [] -> iter inc (move_pos bob_pos x) robo_pos []
    | x0 :: x1 :: xs -> iter inc (move_pos bob_pos x0) (move_pos robo_pos x1) xs
  in
  printf "house with flyers: %d\n" (iter 0 (new_pos 0 0) (new_pos 0 0) (read_data ()))
;;
