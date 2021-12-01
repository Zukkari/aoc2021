open Base

module One : sig
  type t = int list

  val measure : t -> int
end = struct
  type t = int list

  let measure = function
    | [] -> 0
    | x :: xs ->
        let rec aux count last = function
          | [] -> count
          | x :: xs -> if x > last then aux (count + 1) x xs else aux count x xs
        in
        aux 0 x xs
end

module Two : sig
  type t = int list

  val solve : t -> int
end = struct
  type t = int list

  let sum (a, b, c) = a + b + c

  let window =
    let rec aux acc = function
      | x :: x' :: x'' :: xs -> aux ((x, x', x'') :: acc) (x' :: x'' :: xs)
      | _ -> acc
    in

    aux []

  let solve x =
    let open One in
    let windowed = List.rev (window x) in
    List.map ~f:sum windowed |> measure
end
