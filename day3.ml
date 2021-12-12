open Base

module BitCalculator : sig
  type t

  type statistic = { zero : int; one : int }

  val empty : t

  val touch : int list -> t -> t

  val max : t -> int -> int

  val min : t -> int -> int
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

  let max t pos =
    match Map.find t pos with
    | None -> "No value at position " ^ Int.to_string pos |> failwith
    | Some { zero; one } -> if zero > one then 0 else 1

  let min t pos =
    match Map.find t pos with
    | None -> "No value at position " ^ Int.to_string pos |> failwith
    | Some { zero; one } -> if zero > one then 1 else 0
end

module Diagnostic : sig
  type t

  val of_string : string -> t

  val gamma_rate : t list -> t

  val epsilon_rate : t -> t

  val oxygen_gen_rating : t list -> t

  val c02_scrubber_rating : t list -> t

  val to_int : t -> int
end = struct
  open BitCalculator

  type t = int list

  let of_string s =
    List.init (String.length s) ~f:(fun i ->
        String.get s i |> Char.to_string |> Int.of_string)

  let gamma_rate n =
    let open BitCalculator in
    let stats = List.fold ~init:empty ~f:(fun x y -> touch y x) n in

    List.init (List.hd_exn n |> List.length) ~f:(max stats)

  let reverse n =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux ((if Int.equal x 1 then 0 else 1) :: acc) xs
    in
    aux [] n |> List.rev

  let epsilon_rate = reverse

  let calc_rating ~f n =
    let matches_bit bit pos n =
      let filtered = List.filteri ~f:(fun i x -> i = pos && x = bit) n in
      List.length filtered > 0
    in

    let rec aux pos = function
      | [] -> failwith "Empty list"
      | [ x ] -> x
      | n ->
          let stats = List.fold ~init:empty ~f:(fun x y -> touch y x) n in
          let common = f stats pos in
          let next = List.filter ~f:(matches_bit common pos) n in
          aux (pos + 1) next
    in
    aux 0 n

  let oxygen_gen_rating = calc_rating ~f:max

  let c02_scrubber_rating = calc_rating ~f:min

  let to_int (n : int list) =
    List.fold ~init:"0b" ~f:(fun acc x -> acc ^ Int.to_string x) n
    |> Int.of_string
end

let solve gamma epsilon =
  let open Diagnostic in
  let gamma_rate = to_int gamma in
  let epsilon_rate = to_int epsilon in

  gamma_rate * epsilon_rate

