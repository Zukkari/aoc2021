open Base
open Stdio

let solve_pos to_check positions =
  let cost pos positions =
    let distance_cost x =
      List.init x ~f:(fun x -> x + 1) |> List.fold ~init:0 ~f:( + )
    in

    List.map ~f:(fun x -> Int.abs (pos - x) |> distance_cost) positions
    |> List.fold ~init:0 ~f:( + )
  in

  let rec solve_pos costs positions = function
    | [] -> costs
    | x :: xs ->
        let new_costs = Map.set costs ~key:x ~data:(cost x positions) in

        solve_pos new_costs positions xs
  in

  let costs = Map.empty (module Int) in

  solve_pos costs positions to_check

let solve filename =
  let positions =
    In_channel.read_all filename
    |> String.split ~on:',' |> List.map ~f:Int.of_string
  in

  let max =
    List.fold ~init:0 ~f:(fun acc x -> if x > acc then x else acc) positions
  in

  let to_check = List.init max ~f:(fun x -> x + 1) in

  solve_pos to_check positions
  |> Map.fold ~init:None ~f:(fun ~key:_ ~data:x acc ->
         match acc with
         | Some n -> if x < n then Some x else Some n
         | None -> Some x)
