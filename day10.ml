open Base

type result = Valid | Corrupted of char | Incomplete of char list
[@@deriving show]

let is_incomplete = function Incomplete _ -> true | _ -> false

let expected_match = [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

let matches first second =
  List.find
    ~f:(fun (a, b) -> Char.equal a first && Char.equal b second)
    expected_match
  |> Option.is_some

let match_token = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | other -> "Invalid token" ^ String.of_char other |> failwith

let complete seq =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc @ [ match_token x ]) xs
  in
  aux [] seq

let validate program =
  let opening_tokens = List.map ~f:(fun (a, _) -> a) expected_match in

  let is_opening_token t =
    List.find ~f:(fun x -> Char.equal x t) opening_tokens |> Option.is_some
  in

  let rec aux acc = function
    | [] -> Incomplete (complete acc)
    | x :: xs -> (
        if is_opening_token x then aux (x :: acc) xs
        else
          match List.hd acc with
          | Some t when matches t x -> (
              match List.tl acc with Some tl -> aux tl xs | _ -> Valid)
          | _ -> Corrupted x)
  in

  aux [] program

module IO = struct
  let read ~file =
    let open Stdio in
    In_channel.with_file file ~f:(fun inc ->
        In_channel.input_lines inc |> List.map ~f:String.to_list)
end

let solve_p1 ~file =
  let programs = IO.read ~file in

  let to_score = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | c -> "Unexpected token: " ^ String.of_char c |> failwith
  in

  let of_result = function
    | Valid -> 0
    | Incomplete _ -> 0
    | Corrupted x -> to_score x
  in

  List.map ~f:(fun p -> validate p) programs
  |> List.map ~f:of_result |> List.fold ~init:0 ~f:( + )

let solve_p2 ~file =
  let programs = IO.read ~file in

  let extract = function Incomplete seq -> seq | _ -> [] in

  let to_score = function
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | c -> "Unexpected token: " ^ String.of_char c |> failwith
  in

  let scores =
    List.map ~f:validate programs
    |> List.filter ~f:is_incomplete
    |> List.map ~f:extract
    |> List.map ~f:(fun x ->
           List.fold ~init:0 ~f:(fun acc x -> (acc * 5) + to_score x) x)
    |> List.sort ~compare:Int.compare
  in

  List.nth_exn scores (List.length scores / 2)
