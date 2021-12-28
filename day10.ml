open Base

let expected_match = [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

let matches first second =
  List.find
    ~f:(fun (a, b) -> Char.equal a first && Char.equal b second)
    expected_match
  |> Option.is_some

let validate program =
  let opening_tokens = List.map ~f:(fun (a, _) -> a) expected_match in

  let is_opening_token t =
    List.find ~f:(fun x -> Char.equal x t) opening_tokens |> Option.is_some
  in

  let rec aux acc = function
    | [] -> None
    | x :: xs -> (
        if is_opening_token x then aux (x :: acc) xs
        else
          match List.hd acc with
          | Some t when matches t x -> (
              match List.tl acc with Some tl -> aux tl xs | _ -> None)
          | _ -> Some x)
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

  List.map ~f:(fun p -> validate p) programs
  |> List.filter_map ~f:(fun x -> x)
  |> List.map ~f:to_score |> List.fold ~init:0 ~f:( + )
