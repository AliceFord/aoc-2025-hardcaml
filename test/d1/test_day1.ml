open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day1 = Aoc_day1.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

let parse_d1_line l =
  let dir = if int_of_char l.[0] = int_of_char 'L' then 0 else 1 in
  let amount = int_of_string (String.sub l ~pos:1 ~len:(String.length l - 1)) in
  (amount, dir)

let d1_from_file filename =
  let chan = In_channel.create filename in
  let rec loop acc =
    match In_channel.input_line chan with
    | None ->
        In_channel.close chan;
        List.rev acc
    | Some line ->
        loop (parse_d1_line line :: acc)
  in
  loop []

let simple_testbench part (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n d =
    while not (Bits.to_bool !(outputs.ready_for_input)) do
      cycle ();
    done;
    inputs.num_in <--. n;
    inputs.dir_in <--. d;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    (* cycle () *)
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  inputs.part <--. part;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter (d1_from_file "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/d1_partial.txt") ~f:(fun (x, d) -> feed_input x d);
  while not (Bits.to_bool !(outputs.ready_for_input)) do
    cycle ();
  done;
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  while not (Bits.to_bool !(outputs.num_zeros_valid)) do
    cycle ()
  done;
  let num_zeros = Bits.to_unsigned_int !(outputs.num_zeros) in
  print_s [%message "Result" (num_zeros : int)];
  cycle ~n:2 ()
;;

let waves_config =
  Waves_config.to_directory "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/"
  |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Waveform test for part 1" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`Everything
    ~waves_config
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        waves)
    (simple_testbench 0);
  [%expect
    {|
    (Result (num_zeros 3))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │day1$i$clear                ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││            ┌───┐   ┌───────┐   ┌───┐   ┌───┐   ┌───────┐   │
    │                            ││────────────┘   └───┘       └───┘   └───┘   └───┘       └───│
    │day1$i$dir_in               ││                        ┌───────┐       ┌───────┐           │
    │                            ││────────────────────────┘       └───────┘       └───────────│
    │day1$i$finish               ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────┬───────┬───┬───────┬───────┬───────┬───┬───────│
    │day1$i$num_in               ││ 0          │68     │30 │48     │5      │60     │55 │1      │
    │                            ││────────────┴───────┴───┴───────┴───────┴───────┴───┴───────│
    │day1$i$part                 ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$i$start                ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │                            ││────────────────────────────────────┬───────────────────┬───│
    │day1$o$num_zeros            ││ 0                                  │1                  │2  │
    │                            ││────────────────────────────────────┴───────────────────┴───│
    │day1$o$num_zeros_valid      ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$ready_for_input      ││            ┌───┐   ┌───────┐   ┌───┐   ┌───┐   ┌───────┐   │
    │                            ││────────────┘   └───┘       └───┘   └───┘   └───┘       └───│
    │                            ││────────────┬───┬───────┬───┬───────┬───────┬───────┬───┬───│
    │day1$pos                    ││ 0          │50 │82     │52 │0      │95     │55     │0  │99 │
    │                            ││────────────┴───┴───────┴───┴───────┴───────┴───────┴───┴───│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Waveform test for part 2" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`Everything
    ~waves_config
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        waves)
    (simple_testbench 1);
  [%expect
    {|
    (Result (num_zeros 6))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │day1$i$clear                ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │day1$i$data_in_valid        ││            ┌───┐       ┌───────┐   ┌───┐       ┌───┐   ┌───│
    │                            ││────────────┘   └───────┘       └───┘   └───────┘   └───┘   │
    │day1$i$dir_in               ││                            ┌───────┐           ┌───────┐   │
    │                            ││────────────────────────────┘       └───────────┘       └───│
    │day1$i$finish               ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────┬───────────┬───┬───────┬───────────┬───────┬───│
    │day1$i$num_in               ││ 0          │68         │30 │48     │5          │60     │55 │
    │                            ││────────────┴───────────┴───┴───────┴───────────┴───────┴───│
    │day1$i$part                 ││        ┌───────────────────────────────────────────────────│
    │                            ││────────┘                                                   │
    │day1$i$start                ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │                            ││────────────────────┬───────────┬───────┬───┬───────┬───────│
    │day1$o$num_zeros            ││ 0                  │1          │2      │1  │2      │3      │
    │                            ││────────────────────┴───────────┴───────┴───┴───────┴───────│
    │day1$o$num_zeros_valid      ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$ready_for_input      ││            ┌───┐       ┌───────┐   ┌───┐       ┌───┐   ┌───│
    │                            ││────────────┘   └───────┘       └───┘   └───────┘   └───┘   │
    │                            ││────────────┬───┬───┬───────┬───┬───────┬───┬───────┬───────│
    │day1$pos                    ││ 0          │50 │42.│82     │52 │0      │42.│95     │55     │
    │                            ││────────────┴───┴───┴───────┴───┴───────┴───┴───────┴───────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
