open Core

let input = In_channel.read_lines "day1.txt" |> List.map ~f:Int.of_string

let fuel_of_mass m = m / 3 - 2

let day1_1 () =
  List.map ~f:fuel_of_mass input |> List.fold ~init:0 ~f:(+)

let day1_2 () =
  let rec fuels acc m =
    let m' = fuel_of_mass m in
    if m' <= 0 then acc
    else fuels (m' :: acc) m'
  in
  let fuel m = List.fold ~init:0 ~f:(+) (fuels [] m) in
  List.map ~f:fuel input |> List.fold ~init:0 ~f:(+)
