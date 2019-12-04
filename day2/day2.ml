open Core

let input =
  let line = In_channel.read_all "day2.txt" |> String.strip in
  String.split line ~on:',' |> Array.of_list |> Array.map ~f:Int.of_string

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

let run input noun verb =
  input.(1) <- noun;
  input.(2) <- verb;
  operate input 0
