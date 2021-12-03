open Base

type command = Forward of int | Up of int | Down of int

type state = { depth : int; horizontal : int; aim : int }

let initial = { depth = 0; horizontal = 0; aim = 0 }

let reduce_p1 =
  let rec aux s = function
    | [] -> s
    | x :: xs -> (
        match x with
        | Forward d -> aux { s with horizontal = s.horizontal + d } xs
        | Up d -> aux { s with depth = s.depth - d } xs
        | Down d -> aux { s with depth = s.depth + d } xs)
  in
  aux initial

let reduce_p2 =
  let rec aux s = function
    | [] -> s
    | x :: xs -> (
        match x with
        | Forward d ->
            aux
              {
                s with
                horizontal = s.horizontal + d;
                depth = s.depth + (s.aim * d);
              }
              xs
        | Up d -> aux { s with aim = s.aim - d } xs
        | Down d -> aux { s with aim = s.aim + d } xs)
  in
  aux initial

let solve_p1 x =
  let { depth; horizontal; _ } = reduce_p1 x in
  depth * horizontal

let solve_p2 x =
  let { depth; horizontal; _ } = reduce_p2 x in
  depth * horizontal

let parse s =
  let open Base.String in
  match split ~on:' ' s with
  | [ cmd; n ] -> (
      match cmd with
      | cmd when is_prefix ~prefix:"forward" cmd -> Forward (Int.of_string n)
      | cmd when is_prefix ~prefix:"up" cmd -> Up (Int.of_string n)
      | cmd when is_prefix ~prefix:"down" cmd -> Down (Int.of_string n)
      | _ -> assert false)
  | _ -> assert false
