open Base

module Board : sig
  type t

  val to_board : int list list -> t

  val won : t -> bool

  val touch : t -> int -> t

  val unmarked_sum : t -> int

  val debug : t -> unit
end = struct
  type slot = { value : int; marked : bool }

  type t = slot list list

  let to_slot n = { value = n; marked = false }

  let to_board = List.map ~f:(fun row -> List.map ~f:to_slot row)

  let slot_marked s = s.marked

  let rec all_in_row_marked = function
    | [] -> false
    | x :: xs -> List.for_all ~f:slot_marked x || all_in_row_marked xs

  let all_in_column_marked board =
    let column_marked board (pos : int) =
      let marked_at_pos (pos : int) (row : slot list) =
        let marked_at_pos =
          List.filteri ~f:(fun i x -> i = pos && x.marked) row
        in
        List.length marked_at_pos > 0
      in

      List.for_all ~f:(marked_at_pos pos) board
    in

    let columns_stats =
      List.init (List.length board) ~f:(column_marked board)
    in
    List.exists ~f:(Bool.equal true) columns_stats

  let won board = all_in_column_marked board || all_in_row_marked board

  let mark_slot (n : int) = function
    | { value; _ } as slot when value = n -> { slot with marked = true }
    | other -> other

  let touch board n =
    let touch_row n row = List.map ~f:(mark_slot n) row in

    List.map ~f:(touch_row n) board

  let debug (board : t) =
    let debug_row row =
      let open Stdio in
      let values = List.map ~f:(fun s -> Int.to_string s.value) row in

      let s = "[" ^ String.concat ~sep:"," values ^ "]" in
      Out_channel.printf "%s\n" s
    in
    List.iter ~f:debug_row board

  let rec unmarked_sum = function
    | [] -> 0
    | x :: xs ->
        (List.filter ~f:(fun s -> not s.marked) x
        |> List.map ~f:(fun s -> s.value)
        |> List.fold ~init:0 ~f:( + ))
        + unmarked_sum xs
end

module Input : sig
  val read : string -> int list * int list list list
end = struct
  let make_boards lines =
    let boards = ref [] in

    let board = ref [] in

    let _ =
      for i = 0 to Array.length lines - 1 do
        let line = lines.(i) in
        if String.is_empty line then (
          boards := List.rev !board :: !boards;
          board := [])
        else
          let numbers =
            String.split ~on:' ' line
            |> List.filter ~f:(fun s -> String.length s > 0)
            |> List.map ~f:Int.of_string
          in

          board := numbers :: !board
      done
    in
    List.rev !board :: !boards |> List.rev

  let read file =
    let open Stdio in
    let lines = In_channel.read_lines file |> List.to_array in

    let sequence =
      lines.(0) |> String.split ~on:',' |> List.map ~f:Int.of_string
    in

    let boards = Array.sub ~pos:2 ~len:(Array.length lines - 2) lines in

    (sequence, make_boards boards)
end

module Game : sig
  val play : string -> int

  val play2 : string -> int
end = struct
  let rec find_win boards = function
    | [] -> failwith "No possible solution"
    | x :: xs ->
        let new_boards =
          List.map ~f:(fun board -> Board.touch board x) boards
        in

        let won_board = List.filter ~f:Board.won new_boards in

        if List.is_empty won_board then find_win new_boards xs
        else (x, List.hd_exn won_board)

  let rec find_last_win boards =
    let rec find_last_win_acc boards acc = function
      | [] -> List.hd_exn acc
      | x :: xs ->
          let new_boards =
            List.map ~f:(fun board -> Board.touch board x) boards
          in

          let won_boards =
            List.filter ~f:Board.won new_boards |> List.map ~f:(fun b -> (x, b))
          in

          let lost_boards = List.filter ~f:(fun b -> not (Board.won b)) new_boards in

          find_last_win_acc lost_boards (List.append won_boards acc) xs
    in
    find_last_win_acc boards []

  let play filename =
    let seq, board_inputs = Input.read filename in
    let boards = List.map ~f:Board.to_board board_inputs in

    let num, board = find_win boards seq in

    let sum = Board.unmarked_sum board in

    let _ =
      let open Stdio in
      Out_channel.printf "%d %d\n" num sum
    in

    let _ = Board.debug board in

    sum * num

  let play2 filename =
    let seq, board_inputs = Input.read filename in
    let boards = List.map ~f:Board.to_board board_inputs in

    let num, board = find_last_win boards seq in

    let sum = Board.unmarked_sum board in

    let _ =
      let open Stdio in
      Out_channel.printf "%d %d\n" num sum
    in

    let _ = Board.debug board in

    sum * num
end
