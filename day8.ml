open Base

module Solver = struct
  type t = { input : string list; output : string list } [@@deriving show]

  let of_string s =
    match
      String.split ~on:'|' s
      |> List.map ~f:(fun x ->
             String.split ~on:' ' x
             |> List.filter ~f:(fun s -> not (String.is_empty s)))
    with
    | [ i; o ] -> { input = i; output = o }
    | _ -> "Invalid input line " ^ s |> failwith

  let digits = [ 2; 4; 3; 7 ]

  let solve_p1 { output; _ } =
    let matches_digit len =
      List.find ~f:(fun d -> d = len) digits |> Option.is_some
    in

    List.map ~f:String.length output |> List.count ~f:matches_digit

  let solve_p1_all xs = List.map ~f:solve_p1 xs |> List.fold ~init:0 ~f:( + )
end

module Display = struct
  open Solver

  type pixel = { mapping : char } [@@deriving show]

  type display = {
    top_left : pixel;
    top : pixel;
    top_right : pixel;
    middle : pixel;
    bot_left : pixel;
    bot : pixel;
    bot_right : pixel;
  }
  [@@deriving show]

  let rec find_digit len = function
    | [] -> "No number present" |> failwith
    | x :: xs -> if String.length x = len then x else find_digit len xs

  let diff x1 x2 =
    List.filter ~f:(fun x -> not (List.mem ~equal:Char.equal x2 x)) x1

  type mappings = char list [@@deriving show]

  let decode input overlap =
    match
      List.map ~f:(fun code -> diff (String.to_list code) overlap) input
      |> List.filter ~f:(fun code -> List.length code = 1)
      |> List.concat
      |> Set.of_list (module Char)
      |> Set.to_list
    with
    | [ x ] -> x
    | other ->
        "More than 1 match: " ^ show_mappings other ^ " in input "
        ^ String.concat ~sep:";" input
        |> failwith

  let rev_decode input overlap = decode overlap input

  let to_pixel input overlap =
    let mapping = decode input overlap in
    { mapping }

  let display { input; _ } =
    let one = find_digit 2 input in
    let seven = find_digit 3 input in
    let four = find_digit 4 input in
    let eight = find_digit 7 input in

    let known = [ one; seven; four; eight ] in

    let not_decoded =
      List.filter ~f:(fun x -> not (List.mem ~equal:String.equal known x)) input
    in

    let top = to_pixel [ seven ] (String.to_list one) in

    let bot = top.mapping :: String.to_list four |> to_pixel not_decoded in

    let bot_left =
      top.mapping :: bot.mapping :: String.to_list four |> to_pixel [ eight ]
    in

    let middle =
      top.mapping :: bot_left.mapping :: bot.mapping :: String.to_list seven
      |> to_pixel not_decoded
    in

    let top_left =
      top.mapping :: bot_left.mapping :: bot.mapping :: middle.mapping
      :: String.to_list seven
      |> to_pixel not_decoded
    in

    let bot_right =
      [
        top.mapping;
        bot_left.mapping;
        bot.mapping;
        middle.mapping;
        top_left.mapping;
      ]
      |> to_pixel not_decoded
    in

    let top_right =
      [
        top.mapping;
        bot_left.mapping;
        bot.mapping;
        middle.mapping;
        top_left.mapping;
        bot_right.mapping;
      ]
      |> to_pixel [ eight ]
    in

    { top_left; top; top_right; middle; bot_left; bot; bot_right }
end

module Decoder = struct
  open Display

  type position =
    | TopLeft
    | Top
    | TopRight
    | Middle
    | BottomLeft
    | Bottom
    | BottomRight
  [@@deriving show]

  type position_list = position list [@@deriving show]

  let pos code { top_left; top; top_right; middle; bot_left; bot; bot_right } =
    let mappings =
      [
        (top_left, TopLeft);
        (top, Top);
        (top_right, TopRight);
        (middle, Middle);
        (bot_left, BottomLeft);
        (bot, Bottom);
        (bot_right, BottomRight);
      ]
    in
    let code_list = String.to_list code in

    List.filter
      ~f:(fun (code, _) -> List.mem ~equal:Char.equal code_list code.mapping)
      mappings
    |> List.map ~f:(fun (_, m) -> m)

  let decode_pos = function
    | [ TopRight; BottomRight ] -> "1"
    | [ Top; TopRight; Middle; BottomLeft; Bottom ] -> "2"
    | [ Top; TopRight; Middle; Bottom; BottomRight ] -> "3"
    | [ TopLeft; TopRight; Middle; BottomRight ] -> "4"
    | [ TopLeft; Top; Middle; Bottom; BottomRight ] -> "5"
    | [ TopLeft; Top; Middle; BottomLeft; Bottom; BottomRight ] -> "6"
    | [ Top; TopRight; BottomRight ] -> "7"
    | [ TopLeft; Top; TopRight; Middle; BottomLeft; Bottom; BottomRight ] -> "8"
    | [ TopLeft; Top; TopRight; Middle; Bottom; BottomRight ] -> "9"
    | x -> "Invalid digit: " ^ show_position_list x |> failwith

  let decode display code = pos code display |> decode_pos
end

module Solution = struct
  open Solver
  open Display
  open Decoder

  let solve filename =
    let inputs =
      let open Stdio in
      In_channel.with_file filename ~f:(fun inc ->
          In_channel.input_lines inc |> List.map ~f:Solver.of_string)
    in

    let solve_display ({ output; _ } as s) =
      let display = display s in

      List.map ~f:(decode display) output |> String.concat
    in

    List.map ~f:solve_display inputs
    |> List.map ~f:Int.of_string |> List.fold ~init:0 ~f:( + )
end
