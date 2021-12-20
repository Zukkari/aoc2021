open Base

module Permitation : sig
  val permute : 'a list -> 'a list list
end = struct
  let rec permutations l =
    let n = List.length l in
    if n = 1 then [ l ]
    else
      let rec sub e = function
        | [] -> failwith "sub"
        | h :: t -> if h = e then t else h :: sub e t
      in
      let rec aux k =
        let e = List.nth_exn l k in
        let subperms = permutations (sub e l) in
        let t = List.map ~f:(fun a -> e :: a) subperms in
        if k < n - 1 then List.rev_append t (aux (k + 1)) else t
      in
      aux 0

  let permute lst =
    let n = List.length lst in
    let sized = List.init n ~f:(fun x -> x + 1) in
    let permutations = permutations sized in
    List.map
      ~f:(fun perm -> List.map ~f:(fun i -> List.nth_exn lst (i - 1)) perm)
      permutations
end

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

  type display = {
    top_left : string;
    top : string;
    top_right : string;
    middle : string;
    bot_left : string;
    bot : string;
    bot_right : string;
  }
  [@@deriving show]

  let to_display = function
    | [ a; b; c; d; e; f; g ] ->
        {
          top_left = a;
          top = b;
          top_right = c;
          middle = d;
          bot_left = e;
          bot = f;
          bot_right = g;
        }
    | _ -> failwith "to_display"

  let decode { top_left; top; top_right; middle; bot_left; bot; bot_right } =
    String.concat [ top_left; top; top_right; middle; bot_left; bot; bot_right ]

  let displays =
    let variants = [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ] in
    let permutations = Permitation.permute variants in
    List.map ~f:to_display permutations
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

  let fit code { top_left; top; top_right; middle; bot_left; bot; bot_right } =
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
      ~f:(fun (code, _) ->
        List.mem ~equal:Char.equal code_list (Char.of_string code))
      mappings
    |> List.map ~f:(fun (_, m) -> m)

  let decode_pos = function
    | [ TopRight; BottomRight ] -> Some "1"
    | [ Top; TopRight; Middle; BottomLeft; Bottom ] -> Some "2"
    | [ Top; TopRight; Middle; Bottom; BottomRight ] -> Some "3"
    | [ TopLeft; TopRight; Middle; BottomRight ] -> Some "4"
    | [ TopLeft; Top; Middle; Bottom; BottomRight ] -> Some "5"
    | [ TopLeft; Top; Middle; BottomLeft; Bottom; BottomRight ] -> Some "6"
    | [ Top; TopRight; BottomRight ] -> Some "7"
    | [ TopLeft; Top; TopRight; Middle; BottomLeft; Bottom; BottomRight ] ->
        Some "8"
    | [ TopLeft; Top; TopRight; Middle; Bottom; BottomRight ] -> Some "9"
    | [ TopLeft; Top; TopRight; BottomLeft; Bottom; BottomRight ] -> Some "0"
    | _ -> None

  let decode display code = fit code display |> decode_pos

  type s_opt = string option list [@@deriving show]
  type s_list = string list [@@deriving show]

  let find_display displays input =
    let fit acc display =
      match acc with
      | Some d -> Some d
      | None ->
          let all_fitted =
            List.map ~f:(fun code -> decode display code) input
          in

          if List.for_all ~f:Option.is_some all_fitted then Some display
          else None
    in

    List.fold ~init:None ~f:fit displays
end

module Bruteforce = struct
  open Display
  open Decoder

  let solve filename =
    let inputs =
      let open Stdio in
      In_channel.with_file filename ~f:(fun inc ->
          In_channel.input_lines inc |> List.map ~f:Solver.of_string)
    in

    let solve display ~input =
      match display with
      | None -> failwith "solve"
      | Some d ->
          List.map ~f:(fun i -> Option.value_exn (decode d i)) input
          |> String.concat |> Int.of_string
    in

    let displays = Display.displays in

    List.map
      ~f:(fun { input; output } ->
        find_display displays input |> solve ~input:output)
      inputs
    |> List.fold ~init:0 ~f:( + )
end
