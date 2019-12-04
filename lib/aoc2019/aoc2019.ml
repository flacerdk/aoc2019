open Core

let days : (module Day.Day_type.Command) list = [(module Day01.Command); (module Day02.Command)]

let command = Command.group ~summary:"Choose a day" (List.map days ~f:(fun (module Day) -> Day.command))
