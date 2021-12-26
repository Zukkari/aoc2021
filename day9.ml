open Base

let find matrix ri ci =
  let open Option.Let_syntax in
  let%bind row = List.nth matrix ri in
  let%bind point = List.nth row ci in

  Some point

(* Part 1 *)
let is_low_point matrix ri pi p =
  let neighbours =
    [
      find matrix ri (pi - 1) (* Left *);
      find matrix ri (pi + 1) (* Right *);
      find matrix (ri - 1) pi (* Top *);
      find matrix (ri + 1) pi (* Down *);
    ]
    |> List.filter_map ~f:(fun x -> x)
  in
  List.for_all ~f:(fun x -> p < x) neighbours

let find_low_points lst =
  List.mapi ~f:(fun ri row -> List.filteri ~f:(is_low_point lst ri) row) lst
  |> List.concat

(* Part 2 *)

let basin_size matrix ri pi p = 0

let find_basins lst =
  List.mapi ~f:(fun ri row -> List.mapi ~f:(basin_size lst ri) row) lst
  |> List.concat

(* IO *)
module IO = struct
  let of_string line =
    String.to_list line
    |> List.map ~f:(fun c -> Char.to_string c |> Int.of_string)

  let read ~file =
    let open Stdio in
    In_channel.with_file file ~f:(fun inc ->
        In_channel.input_lines inc |> List.map ~f:of_string)
end

let solve_p1 ~file =
  let matrix = IO.read ~file in

  find_low_points matrix |> List.map ~f:(( + ) 1) |> List.fold ~init:0 ~f:( + )

let rec replace_first target n = function
  | [] -> []
  | x :: xs -> if x = target then n :: xs else x :: replace_first target n xs

let find_max_n n acc x =
  if List.length acc < n then x :: acc
  else
    match List.find ~f:(fun elem -> elem < x) acc with
    | None -> acc
    | Some n -> replace_first n x acc

let solve_p2 ~file =
  let matrix = IO.read ~file in

  let basins = find_basins matrix |> List.fold ~init:[] ~f:(find_max_n 3) in

  List.fold ~init:1 ~f:( * ) basins
