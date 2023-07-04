open Core

let read_data () = In_channel.read_all "data.txt" |> String.strip |> String.split_lines

let paper_area l w h =
  let lw = l * w in
  let wh = w * h in
  let hl = h * l in
  let smallest_side =
    List.hd_exn @@ List.stable_sort [ lw; wh; hl ] ~compare:Int.compare
  in
  (2 * lw) + (2 * wh) + (2 * hl) + smallest_side
;;

let ribbon_length l w h =
  let order_by_smallest = List.stable_sort [ l; w; h ] ~compare:Int.compare in
  let x = List.hd_exn order_by_smallest in
  let y = List.nth_exn order_by_smallest 1 in
  x + x + y + y + (l * w * h)
;;

let resources_needed s =
  let nums = String.split s ~on:'x' |> List.map ~f:(fun x -> Int.of_string x) in
  match nums with
  | [ l; w; h ] -> paper_area l w h, ribbon_length l w h
  | _ -> failwithf "input does not match expected pattern: %s" s ()
;;

let () =
  read_data ()
  |> List.map ~f:resources_needed
  |> List.fold ~init:(0, 0) ~f:(fun (x1, x2) (y1, y2) -> x1 + y1, x2 + y2)
  |> function
  | paper, ribbon -> printf "total paper: %d\ntotal ribbon: %d" paper ribbon
;;
