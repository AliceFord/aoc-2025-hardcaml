open! Core
open! Hardcaml
open! Signal

let num_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; part : 'a
    ; num_in : 'a [@bits num_bits]
    ; dir_in : 'a [@bits 1]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    {
      num_zeros : 'a [@bits num_bits];
      num_zeros_valid : 'a [@bits 1];
      ready_for_input : 'a [@bits 1]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs1
    | Mod100_1
    | Done1
    | Accepting_inputs2
    | Mod100_2
    | Done2
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; part; num_in; dir_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in

  let%hw_var pos = Variable.reg spec ~width:num_bits in

  let num_zeros = Variable.reg spec ~width:num_bits in
  let num_zeros_valid = Variable.reg spec ~width:1 in
  let ready_for_input = Variable.reg spec ~width:1 in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start [
                if_ part [
                  pos <--. 50;
                  num_zeros <-- zero num_bits;
                  num_zeros_valid <-- gnd;
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs2
                ] [
                  pos <--. 50;
                  num_zeros <-- zero num_bits;
                  num_zeros_valid <-- gnd;
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs1
                ]
              ]
            ] )
        ; ( Accepting_inputs1
          , [ when_ data_in_valid [ 
                ready_for_input <-- gnd; (* no longer ready for input *)
                if_ (dir_in ==:. 1) [
                  if_ (pos.value +: num_in >=+. 100) [
                    pos <-- pos.value +: num_in -:. 100;
                    sm.set_next Mod100_1;
                  ] [
                    pos <-- pos.value +: num_in;
                    ready_for_input <-- vdd; (* no modulo needed *)
                  ];
                ] [
                  if_ (num_in >+ pos.value) [
                    pos <-- pos.value -: num_in +:. 100;
                    sm.set_next Mod100_1;
                  ] [
                    pos <-- pos.value -: num_in;
                    ready_for_input <-- vdd; (* no modulo needed *)
                  ];
                ];

                when_ (pos.value ==:. 0) [
                  num_zeros <-- (num_zeros.value +:. 1);
                ]
              ]
            ; when_ finish [ sm.set_next Done1 ]
            ] )
        ; ( Mod100_1
          , [ 
              if_ (pos.value >=+. 100) [
                pos <-- pos.value -:. 100;
              ] [
                if_ (pos.value <+. 0) [
                  pos <-- pos.value +:. 100;
                ] [
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs1;
                ]
              ];
            ] )
        ; ( Done1
          , [
              when_ (pos.value ==:. 0) [ (* final check *)
                num_zeros <-- (num_zeros.value +:. 1);
              ];
              num_zeros_valid <-- vdd;
              sm.set_next Idle
            ] )
        ; ( Accepting_inputs2
          , [ when_ data_in_valid [ 
                ready_for_input <-- gnd; (* no longer ready for input *)
                if_ (dir_in ==:. 1) [
                  if_ (pos.value +: num_in >=+. 100) [
                    pos <-- pos.value +: num_in -:. 100;
                    num_zeros <-- (num_zeros.value +:. 1);
                    sm.set_next Mod100_2;
                  ] [
                    pos <-- pos.value +: num_in;
                    ready_for_input <-- vdd; (* no modulo needed *)
                  ];
                ] [
                  if_ (num_in >+ pos.value) [
                    pos <-- pos.value -: num_in;
                    when_ (pos.value ==:. 0) [num_zeros <-- (num_zeros.value -:. 1)]; (* to avoid double counting this 0 *)
                    sm.set_next Mod100_2;
                  ] [
                    pos <-- pos.value -: num_in;
                    when_ (pos.value -: num_in ==:. 0) [
                      num_zeros <-- (num_zeros.value +:. 1); (* we count left 0's immediately. *)
                    ];
                    ready_for_input <-- vdd; (* no modulo needed *)
                  ];
                ];
              ]
            ; when_ finish [ sm.set_next Done2 ]
            ] )
        ; ( Mod100_2
          , [ 
              if_ (pos.value >=+. 100) [
                pos <-- pos.value -:. 100;
                num_zeros <-- (num_zeros.value +:. 1);
              ] [
                if_ (pos.value <+. 0) [
                  pos <-- pos.value +:. 100;
                  num_zeros <-- (num_zeros.value +:. 1);
                  when_ (pos.value +:. 100 ==:. 0) [ (* jumping right FROM 0 TO 0 so need to add 2 in total *)
                    num_zeros <-- (num_zeros.value +:. 2);
                  ];
                ] [
                  ready_for_input <-- vdd;
                  sm.set_next Accepting_inputs2;
                ]
              ];
            ] )
        ; ( Done2
          , [
              num_zeros_valid <-- vdd;
              sm.set_next Idle
            ] )
        ]
    ];
  { 
    num_zeros = num_zeros.value;
    num_zeros_valid = num_zeros_valid.value;
    ready_for_input = ready_for_input.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
