open Base

module Simulation = struct
  type t = {
    zero : int;
    one : int;
    two : int;
    three : int;
    four : int;
    five : int;
    six : int;
    seven : int;
    eight : int;
  }
  [@@deriving show]

  let of_string s =
    let count i = List.count ~f:(fun x -> x = i) in
    let initial = String.split ~on:',' s |> List.map ~f:Int.of_string in
    {
      zero = count 0 initial;
      one = count 1 initial;
      two = count 2 initial;
      three = count 3 initial;
      four = count 4 initial;
      five = count 5 initial;
      six = count 6 initial;
      seven = count 7 initial;
      eight = count 8 initial;
    }

  let iterate = function
    | { zero; one; two; three; four; five; six; seven; eight } ->
        let new_fish = zero in
        let move_to_six = zero in

        {
          zero = one;
          one = two;
          two = three;
          three = four;
          four = five;
          five = six;
          six = seven + move_to_six;
          seven = eight;
          eight = new_fish;
        }

  let rec simulate ~n s = if n = 0 then s else simulate ~n:(n - 1) (iterate s)

  let count = function
    | { zero; one; two; three; four; five; six; seven; eight } ->
        zero + one + two + three + four + five + six + seven + eight
end
