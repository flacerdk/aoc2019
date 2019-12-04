open Core

module Solution : Day.Day_type.Solution = struct
  let day = 2

  let step prog ip =
    (match prog.(ip) with
     | 1 -> prog.(prog.(ip + 3)) <- prog.(prog.(ip + 1)) + prog.(prog.(ip + 2));
     | 2 -> prog.(prog.(ip + 3)) <- prog.(prog.(ip + 1)) * prog.(prog.(ip + 2));
     | _ -> (););
    prog

  let rec operate prog ip =
    if prog.(ip) = 99
    then prog
    else operate (step prog ip) (ip + 4)

  let day2_1 input =
    input.(1) <- 12;
    input.(2) <- 2;
    (operate input 0).(0)

  let day2_2 input =
    let rec run input noun verb =
      input.(1) <- noun;
      input.(2) <- verb;
      let output = (operate (Array.copy input) 0).(0) in
      if output = 19690720
      then 100 * noun + verb
      else if verb >= 99 then (if noun >= 99 then -1 else run input (noun + 1) 0) else run input noun (verb + 1) in
    run input 0 0

  let solve input_string ~part =
    let input = String.strip input_string |> String.split ~on:',' |> Array.of_list |> Array.map ~f:Int.of_string in
    let out = match part with
      | Day.One -> day2_1 (Array.copy input)
      | Day.Two -> day2_2 input in
    sprintf "%d" out
end

module Command = Day.Solution.Make (Solution)
