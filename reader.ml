open Stdio

let read name ~f =
  In_channel.with_file name ~f:(fun inc ->
      In_channel.input_lines inc |> List.map f)
