open Core

let parse input_string = String.split input_string ~on:',' |> Array.of_list |> Array.map ~f:Int.of_string

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

let rec run input noun verb =
  input.(1) <- noun;
  input.(2) <- verb;
  let output = (operate (Array.copy input) 0).(0) in
  if output = 19690720
  then 100 * noun + verb
  else if verb >= 99 then (if noun >= 99 then -1 else run input (noun + 1) 0) else run input noun (verb + 1)

let solve_1 input_string =
  let input = parse input_string in
  Int.to_string ((operate input 0).(0))

let solve_2 input_string =
  let input = parse input_string in
  run input 0 0

let%expect_test "Solve example" =
  print_string (solve_1 "1,9,10,3,2,3,11,0,99,30,40,50");
  [%expect {| 3500 |}]
