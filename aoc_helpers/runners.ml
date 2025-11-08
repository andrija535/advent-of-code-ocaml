type runner_configuration = {
  day: int;
  input_file: string
}

let day = ref 1
let input_file = ref None

let arg_spec = [
  ("-day", Arg.Set_int day, "Specify the day to run");
  ("-input-file", Arg.String (fun s -> input_file := Some s), "Specify the input file")
]

let usage_message exec_name = Printf.sprintf "Usage: %s [-day N] [-input-file FILE]" exec_name

let parse_args (exec_name: string): unit =
  Arg.parse 
    arg_spec 
    (Fun.const ()) @@
    usage_message exec_name

let get_config (exec_name: string) : runner_configuration =
  let input_file =
    Option.map (fun input_file ->
      { day = !day; input_file }
    ) !input_file 
  in
  match input_file with
  | None -> Printf.eprintf "%s\n" (usage_message exec_name); exit 1
  | Some config -> config

let run ~(exec_name: string) ~(day_mapping: int -> string Seq.t -> 'a) ~(printer: Format.formatter -> 'a -> unit): unit =
  parse_args exec_name;
  let runner_config = get_config exec_name in
  Io.read_lines runner_config.input_file 
  |> day_mapping runner_config.day 
  |> Format.printf "Result of day %d: %a\n" runner_config.day printer 
