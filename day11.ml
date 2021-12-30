open Base

type state = Flashed | Charging [@@deriving show]

type octopus = { energy_level : int; state : state } [@@deriving show]

type octopus_list = octopus list [@@deriving show]

type int_list = int list [@@deriving show]

let empty = { energy_level = 0; state = Charging }

let is_charging = function { state = Charging; _ } -> true | _ -> false

let has_flashed = function { state = Flashed; _ } -> true | _ -> false

let to_octopus n = { energy_level = n; state = Charging }

let grid_size = 10

let pos row col = (row * grid_size) + col

let of_index i =
  let row = i / grid_size in
  let col = i - (row * grid_size) in
  (row, col)

let increment =
  List.map ~f:(fun oct -> { oct with energy_level = oct.energy_level + 1 })

let flash seq =
  let expand seq =
    let exp row col =
      [
        (row + 1, col);
        (row - 1, col);
        (row, col + 1);
        (row, col - 1);
        (row - 1, col - 1);
        (row + 1, col + 1);
        (row - 1, col + 1);
        (row + 1, col - 1);
      ]
    in

    let to_pos i =
      let row, col = of_index i in
      exp row col
    in

    List.map ~f:to_pos seq |> List.concat |> List.map ~f:(fun (r, c) -> pos r c)
  in

  let adjust = function
    | { energy_level; state = Charging; _ } when energy_level > 9 ->
        { energy_level; state = Flashed }
    | other -> other
  in

  let flash_pos s =
    List.mapi
      ~f:(fun i oct ->
        if oct.energy_level > 9 && is_charging oct then Some i else None)
      s
    |> List.filter_map ~f:(fun x -> x)
  in

  let rec aux seq visited = function
    | [] -> seq
    | x :: xs ->
        let new_seq =
          List.mapi ~f:(fun i oct -> if x = i then adjust oct else oct) seq
        in

        let checked = x :: visited in

        let queued = checked @ xs in

        let flashed =
          flash_pos new_seq |> expand
          |> List.filter ~f:(fun p -> not (List.mem ~equal:Int.equal queued p))
        in

        aux new_seq checked (xs @ flashed)
  in

  aux seq [] (flash_pos seq)

let count = List.count ~f:has_flashed

let reset =
  let reset_octopus = function { state = Flashed; _ } -> empty | o -> o in

  List.map ~f:reset_octopus

let rec iterate seq = function
  | 0 -> 0
  | n ->
      let incremented = increment seq in
      let flashed = flash incremented in
      let count = count flashed in

      let () =
        let open Stdio in
        Out_channel.print_endline (show_octopus_list flashed)
      in

      count + iterate (reset flashed) (n - 1)

module IO = struct
  let read_input ~file =
    let open Stdio in
    let of_string line =
      String.to_list line
      |> List.map ~f:(fun c -> Char.to_string c |> Int.of_string |> to_octopus)
    in
    In_channel.with_file file ~f:(fun inc ->
        In_channel.input_lines inc |> List.map ~f:of_string)
    |> List.concat
end
