open Base

module Board : sig
  type t

  val empty : int -> t
  val iterate : t -> (int * int) list -> t
  val overlap : t -> int
  val show : t -> string
end = struct
  type t = (int * int * int) list [@@deriving show]

  let empty size =
    let range = List.init size ~f:(fun x -> x + 1) in
    List.cartesian_product (0 :: range) (0 :: range)
    |> List.map ~f:(fun (x, y) -> (x, y, 0))

  let rec apply p =
    let rec aux (x, y) acc = function
      | [] -> acc
      | ((x1, y1, z) as p) :: xs ->
          if x = x1 && y = y1 then (acc @ [ (x1, y1, z + 1) ]) @ xs
          else aux (x, y) (acc @ [ p ]) xs
    in
    aux p []

  let rec iterate board = function
    | [] -> board
    | x :: xs -> iterate (apply x board) xs

  let rec overlap = function
    | [] -> 0
    | (_, _, z) :: xs -> if z >= 2 then 1 + overlap xs else overlap xs
end

module Bresenham : sig
  val plotLine : int * int -> int * int -> (int * int) list
end = struct
  let plotLineLow (x0, y0) (x1, y1) =
    let dx = x1 - x0 in

    let dy = if y1 - y0 < 0 then -(y1 - y0) else y1 - y0 in

    let yi = if y1 - y0 < 0 then -1 else 1 in

    let d = ref ((2 * dy) - dx) in

    let y = ref y0 in

    let lines = ref [] in

    let () =
      for i = x0 to x1 do
        lines := !lines @ [ (i, !y) ];
        if !d > 0 then (
          y := !y + yi;
          d := !d + (2 * (dy - dx)))
        else d := !d + (2 * dy)
      done
    in

    !lines

  let plotLineHigh (x0, y0) (x1, y1) =
    let dx = if x1 - x0 < 0 then -(x1 - x0) else x1 - x0 in

    let dy = y1 - y0 in

    let xi = if x1 - x0 < 0 then -1 else 1 in

    let d = ref ((2 * dx) - dy) in

    let x = ref x0 in

    let lines = ref [] in

    let () =
      for i = y0 to y1 do
        lines := !lines @ [ (!x, i) ];
        if !d > 0 then (
          x := !x + xi;
          d := !d + (2 * (dx - dy)))
        else d := !d + (2 * dx)
      done
    in

    !lines

  let plotLine (x0, y0) (x1, y1) =
    if Int.abs (y1 - y0) < Int.abs (x1 - x0) then
      if x0 > x1 then plotLineLow (x1, y1) (x0, y0)
      else plotLineLow (x0, y0) (x1, y1)
    else if y0 > y1 then plotLineHigh (x1, y1) (x0, y0)
    else plotLineHigh (x0, y0) (x1, y1)
end

module Input : sig
  val read : string -> ((int * int) * (int * int)) list
end = struct
  let transform line =
    let regex =
      Str.regexp "\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)"
    in

    if Str.string_match regex line 0 then
      ( ( Str.matched_group 1 line |> Int.of_string,
          Str.matched_group 2 line |> Int.of_string ),
        ( Str.matched_group 3 line |> Int.of_string,
          Str.matched_group 4 line |> Int.of_string ) )
    else "Broken line: " ^ line |> failwith

  let read name =
    let open Stdio in
    In_channel.with_file name ~f:(fun inc ->
        In_channel.input_lines inc |> List.map ~f:transform)
end

module TupleMap = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

let solve steps =
  let cache = Map.empty (module TupleMap) in

  let group_by m line =
    match Map.find m line with
    | Some n -> Map.set m ~key:line ~data:(n + 1)
    | None -> Map.set m ~key:line ~data:1
  in

  List.map ~f:(fun (start, dest) -> Bresenham.plotLine start dest) steps
  |> List.join
  |> List.fold ~init:cache ~f:group_by
  |> Map.count ~f:(fun v -> v >= 2)

let solve_p1 filename =
  let steps =
    Input.read filename
    |> List.filter ~f:(fun ((x0, y0), (x1, y1)) -> x0 = x1 || y0 = y1)
  in

  solve steps

let solve_p2 filename =
  let steps = Input.read filename in
  solve steps
