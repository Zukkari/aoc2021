open Base

module BitCalculator : sig
  type t

  type statistic = { zero : int; one : int }

  val empty : t

  val touch : int list -> t -> t

  val max : int -> t -> int
end = struct
  type statistic = { zero : int; one : int }

  type t = (int, statistic, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let touch n t =
    let rec stats t pos = function
      | [] -> t
      | x :: xs ->
          let is_zero = Int.equal x 0 in
          let with_latest =
            match Map.find t pos with
            | Some n ->
                let updated =
                  {
                    zero = (n.zero + if is_zero then 1 else 0);
                    one = (n.one + if is_zero then 0 else 1);
                  }
                in
                Map.set t ~key:pos ~data:updated
            | None ->
                let stats =
                  {
                    zero = (if is_zero then 1 else 0);
                    one = (if is_zero then 0 else 1);
                  }
                in
                Map.set t ~key:pos ~data:stats
          in
          stats with_latest (pos + 1) xs
    in
    stats t 0 n

  let max pos t =
    match Map.find t pos with
    | None -> 0
    | Some { zero; one } -> if zero > one then 0 else 1
end

module Diagnostic : sig
  type t

  val of_string : string -> t

  val gamma_rate : t list -> t

  val epsilon_rate : t -> t

  val to_int : t -> int
end = struct
  type t = int list

  let of_string s =
    List.init (String.length s) ~f:(fun i ->
        String.get s i |> Char.to_string |> Int.of_string)

  let gamma_rate n =
    let open BitCalculator in
    let stats = List.fold ~init:empty ~f:(fun x y -> touch y x) n in

    List.init (List.hd_exn n |> List.length) ~f:(fun index -> max index stats)

  let epsilon_rate n =
    let reverse n =
      let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux ((if Int.equal x 1 then 0 else 1) :: acc) xs
      in
      aux [] n
    in
    reverse n |> List.rev

  let to_int (n : int list) =
    List.fold ~init:"0b" ~f:(fun acc x -> acc ^ Int.to_string x) n
    |> Int.of_string
end

let solve gamma epsilon =
  let open Diagnostic in
  let gamma_rate = to_int gamma in
  let epsilon_rate = to_int epsilon in

  gamma_rate * epsilon_rate
