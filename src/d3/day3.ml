open! Core
open! Hardcaml
open! Signal

let num_bits = 16

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits num_bits]
    ; data_sep : 'a [@bits 1]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    {
      sum : 'a With_valid.t [@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Input_sep
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_sep; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in
  let%hw_var max_so_far = Variable.reg spec ~width:num_bits in  (* 4 is all that is necssary *)
  let%hw_var snd_max_so_far = Variable.reg spec ~width:num_bits in

  let sum = Variable.reg spec ~width:32 in
  let sum_valid = Variable.reg spec ~width:1 in

  (* logic: store max and second max seen thus far. update. sum. *)

  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ max_so_far <-- zero num_bits;
                  snd_max_so_far <-- zero num_bits;
                  sum <-- zero 32;
                  sum_valid <-- gnd;
                  sm.set_next Accepting_inputs;
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_ data_in_valid [
                if_ (snd_max_so_far.value >: max_so_far.value) [
                  max_so_far <-- snd_max_so_far.value;
                  snd_max_so_far <-- data_in;
                ] [
                  when_ (data_in >: snd_max_so_far.value) [
                    snd_max_so_far <-- data_in;
                  ]
                ]
              ];
              when_ data_sep [sm.set_next Input_sep];
              when_ finish [ sm.set_next Done ]
            ] )
        ; ( Input_sep
          , [ 
              sum <-- sum.value +: (max_so_far.value *: (of_int_trunc 10 ~width:16) +: (uresize snd_max_so_far.value ~width:32));
              max_so_far <-- zero num_bits;
              snd_max_so_far <-- zero num_bits;
              sm.set_next Accepting_inputs;
            ] )
        ; ( Done
          , [ 
              when_ (max_so_far.value >:. 0) [
                sum <-- sum.value +: (max_so_far.value *: (of_int_trunc 10 ~width:16) +: (uresize snd_max_so_far.value ~width:32));
                max_so_far <-- zero num_bits;
                snd_max_so_far <-- zero num_bits;
              ];

              sum_valid <-- vdd;
              when_ finish [ sm.set_next Accepting_inputs ]
            ] )
        ]
    ];
  { 
    sum = { value = sum.value; valid = sum_valid.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day3" create
;;
