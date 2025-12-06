open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Range_finder = Hardcaml_demo_project.Range_finder
module Harness = Cyclesim_harness.Make (Range_finder.I) (Range_finder.O)

let ( <--. ) = Bits.( <--. )
(* let sample_input_values = [ (68, 0); (30, 0); (48, 1); (5, 0); (60, 1); (55, 0); (1, 0); (99, 0); (14, 1); (82, 0) ] *)

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

let simple_testbench (sim : Harness.Sim.t) =
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
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter (d1_from_file "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/d1.txt") ~f:(fun (x, d) -> feed_input x d);
  while not (Bits.to_bool !(outputs.ready_for_input)) do
    cycle ();
  done;
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.num_zeros.valid)) do
    cycle ()
  done;
  let num_zeros = Bits.to_unsigned_int !(outputs.num_zeros.value) in
  print_s [%message "Result" (num_zeros : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

let waves_config =
  Waves_config.to_directory "/mnt/c/Users/olive/Documents/coding/aoc2025/hardcaml_template_project/"
  |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Range_finder.hierarchical simple_testbench;
  [%expect {| (Result (num_zeros 1)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "range_finder*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Range_finder.hierarchical
    ~trace:`Everything
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    (Result (range 146))
    ┌
    |}]
;;
