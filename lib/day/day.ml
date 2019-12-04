open Core

type part = One | Two [@@deriving show]

module Day_type = struct
  module type Solution = sig
    val day : int
    val solve : string -> part:part -> string
  end

  module type Command = sig
    val command : string * Command.t
  end
end

module type Solution = sig
  module Make : functor (Day : Day_type.Solution) -> Day_type.Command
end

module Solution = struct
  module Make (Day : Day_type.Solution) = struct
    let command =
      let day_string = sprintf "%d" Day.day in
      let input = In_channel.read_all (sprintf "./input/day%02d.txt" Day.day) in
      let f part = (show_part part, Command.basic
                           ~summary:(sprintf "part  solution")
                           (Command.Param.return (fun () -> printf "%s\n" (Day.solve input ~part:part)))) in
      (day_string, Command.group ~summary:(sprintf "day %s solutions" day_string)
        (List.map [One; Two] ~f:f))
  end
end
