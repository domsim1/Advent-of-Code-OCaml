open Base

let find_offset code =
  let rec iter i =
    let offset_code = String.concat [ code; Int.to_string i ] in
    if Md5_lib.string offset_code |> Md5_lib.to_hex |> String.is_prefix ~prefix:"00000"
    then i
    else iter (i + 1)
  in
  iter 1
;;

let code = "ckczppom"
let () = find_offset code |> Int.to_string |> Stdlib.print_endline
