open Core

module Point = struct
  type t = (int * int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
  let manhattan_distance (p1, p2) = abs p1 + abs p2
end

type direction = Up | Down | Right | Left

let parse_move move_string =
  let m = Re.exec (Re.Pcre.regexp "([A-Z])([0-9]+)") move_string in
  let direction = Re.Group.get m 1 in
  let steps = Int.of_string (Re.Group.get m 2) in
  match direction with
  | "U" -> (Up, steps)
  | "D" -> (Down, steps)
  | "L" -> (Left, steps)
  | "R" -> (Right, steps)
  | _ -> failwith "Invalid input"

let perform_step (x, y) direction =
  match direction with
  | Up -> (x, y + 1)
  | Down -> (x, y - 1)
  | Right -> (x + 1, y)
  | Left -> (x - 1, y)

let perform_move p move =
  let rec perform_move_inner p (direction, steps) accum =
    match (direction, steps) with
    | (_, 0) -> accum
    | (d, s) ->
       let step = perform_step p d in
       perform_move_inner step (d, s - 1) (step :: accum) in
  perform_move_inner p move []

let parse_wire line =
  String.split ~on:',' line
  |> List.map ~f:parse_move

let parse input_string =
  let parsed = String.split ~on:'\n' input_string in
  (parse_wire @@ List.hd_exn parsed,
   parse_wire @@ List.hd_exn @@ List.tl_exn parsed)

let all_points wire =
  List.fold ~init:[(0, 0)] ~f:(fun accum move ->
      let start_point = List.hd_exn accum in
      perform_move start_point move @ accum
    ) wire
  |> List.rev

let make_table wire =
  let points = all_points wire in
  let table = Hashtbl.create (module Point) in
  List.iteri ~f:(fun i p ->
      let f = function
        | Some steps -> Some (i :: steps)
        | None -> Some [i] in
      Point.Table.change table p ~f:f) points;
  table


let intersections wire1 wire2 =
  let (table1, table2) = (make_table wire1, make_table wire2) in
  let (set1, set2) = (Point.Set.of_list @@ Point.Table.keys table1,
                      Point.Set.of_list @@ Point.Table.keys table2) in
  let points = Point.Set.inter set1 set2
               |> Point.Set.filter ~f:(fun p -> not (Point.equal p (0, 0))) in
  let get_table = Point.Table.filter_keys ~f:(fun p -> Point.Set.mem points p) in
  (get_table table1, get_table table2)

let solve_1 input_string =
  let (wire1, wire2) = parse input_string in
  let (table, _) = intersections wire1 wire2 in
  Point.Table.keys table
  |> List.map ~f:(fun p -> Point.manhattan_distance p)
  |> List.min_elt ~compare:(fun a b -> if a < b then (-1) else 1)
  |> function
    | Some s -> Int.to_string s
    | None -> failwith "Invalid argument"

let%expect_test "Solve example" =
  print_endline (solve_1 "R8,U5,L5,D3
                          U7,R6,D4,L4");
  print_endline (solve_1 "R75,D30,R83,U83,L12,D49,R71,U7,L72
                          U62,R66,U55,R34,D71,R55,D58,R83");
  print_endline @@ solve_1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                            U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
  [%expect {|
            6
            159
            135
            |}]
