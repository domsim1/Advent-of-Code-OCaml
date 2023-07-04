open Core

let read_data () = In_channel.read_all "data.txt" |> String.strip |> String.split_lines

module Grid = struct
  type command =
    | ON
    | OFF
    | TOGGLE

  type instruction =
    { command : command
    ; start_x : int
    ; start_y : int
    ; end_x : int
    ; end_y : int
    }

  type grid =
    { data : int array
    ; width : int
    }

  let create width height = { data = Array.create ~len:(width * height) 0; width }

  let set_range grid sx sy ex ey v =
    for row = sx to ex do
      for col = sy to ey do
        grid.data.((grid.width * row) + col) <- v
      done
    done
  ;;

  let on grid command =
    set_range grid command.start_x command.start_y command.end_x command.end_y 1
  ;;

  let off grid command =
    set_range grid command.start_x command.start_y command.end_x command.end_y 0
  ;;

  let increase_brightness grid command v =
    for row = command.start_x to command.end_x do
      for col = command.start_y to command.end_y do
        let i = (grid.width * row) + col in
        let v = grid.data.(i) + v in
        grid.data.(i) <- (if v < 0 then 0 else v)
      done
    done
  ;;

  let decrease_brightness grid command =
    for row = command.start_x to command.end_x do
      for col = command.start_y to command.end_y do
        let i = (grid.width * row) + col in
        let v = grid.data.(i) - 1 in
        grid.data.(i) <- (if v < 0 then 0 else v)
      done
    done
  ;;

  let toogle grid command =
    for row = command.start_x to command.end_x do
      for col = command.start_y to command.end_y do
        let i = (grid.width * row) + col in
        grid.data.(i) <- 1 - grid.data.(i)
      done
    done
  ;;

  let lights_on grid = Array.count grid.data ~f:(fun v -> v = 1)
  let total_brightness grid = Array.fold grid.data ~init:0 ~f:(fun a b -> a + b)
end

let parse_instruction ins_s =
  let open Grid in
  match String.split_on_chars ins_s ~on:[ ' '; ',' ] with
  | [ "turn"; "on"; sx; sy; "through"; ex; ey ] ->
    { command = ON
    ; start_x = Int.of_string sx
    ; start_y = Int.of_string sy
    ; end_x = Int.of_string ex
    ; end_y = Int.of_string ey
    }
  | [ "turn"; "off"; sx; sy; "through"; ex; ey ] ->
    { command = OFF
    ; start_x = Int.of_string sx
    ; start_y = Int.of_string sy
    ; end_x = Int.of_string ex
    ; end_y = Int.of_string ey
    }
  | [ "toggle"; sx; sy; "through"; ex; ey ] ->
    { command = TOGGLE
    ; start_x = Int.of_string sx
    ; start_y = Int.of_string sy
    ; end_x = Int.of_string ex
    ; end_y = Int.of_string ey
    }
  | _ -> failwithf "not valid instruction: %s" ins_s ()
;;

let update_grid grid ins_s =
  let ins = parse_instruction ins_s in
  match ins.command with
  | ON -> Grid.on grid ins
  | OFF -> Grid.off grid ins
  | TOGGLE -> Grid.toogle grid ins
;;

let update_grid_new grid ins_s =
  let ins = parse_instruction ins_s in
  match ins.command with
  | ON -> Grid.increase_brightness grid ins 1
  | OFF -> Grid.decrease_brightness grid ins
  | TOGGLE -> Grid.increase_brightness grid ins 2
;;

let () =
  let grid = Grid.create 1000 1000 in
  let rec loop = function
    | [] -> ()
    | x :: xs ->
      update_grid grid x;
      loop xs
  in
  loop @@ read_data ();
  Grid.lights_on grid |> Int.to_string |> print_endline;
  let grid = Grid.create 1000 1000 in
  let rec loop = function
    | [] -> ()
    | x :: xs ->
      update_grid_new grid x;
      loop xs
  in
  loop @@ read_data ();
  Grid.total_brightness grid |> Int.to_string |> print_endline
;;
