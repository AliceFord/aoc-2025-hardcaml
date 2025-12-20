open! Core
open! Hardcaml

module Day3 = Aoc_day3.Day3

let generate_range_finder_rtl () =
  let module C = Circuit.With_interface (Day3.I) (Day3.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"range_finder_top" (Day3.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Vhdl [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let range_finder_rtl_command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_range_finder_rtl ()]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "range-finder", range_finder_rtl_command ])
;;
