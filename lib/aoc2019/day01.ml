open Core

module Solution : Day.Day_type.Solution = struct
  let day = 1

  let fuel_of_mass m = m / 3 - 2

  let day1_1 input =
    List.map ~f:fuel_of_mass input |> List.fold ~init:0 ~f:(+)

  let day1_2 input =
    let rec fuels acc m =
      let m' = fuel_of_mass m in
      if m' <= 0 then acc
      else fuels (m' :: acc) m'
    in
    let fuel m = List.fold ~init:0 ~f:(+) (fuels [] m) in
    List.map ~f:fuel input |> List.fold ~init:0 ~f:(+)

  let solve input_string ~part =
    let input = String.split input_string ~on:'\n' |> List.map ~f:Int.of_string in
    let out = match part with
    | Day.One -> day1_1 input
    | Day.Two -> day1_2 input in
    sprintf "%d" out
end

module Command = Day.Solution.Make (Solution)
