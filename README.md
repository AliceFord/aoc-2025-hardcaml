# AoC Solutions in Hardcaml

### A prelude

First time writing anything at all for hardware, was extremely fun to learn! Would be keen to try on real hardware at some point.

Based on (https://github.com/janestreet/hardcaml_template_project/)[a HardCaml template by Jane Street].

## How to run:

Install Hardcaml:

```
opam install -y hardcaml hardcaml_test_harness hardcaml_waveterm ppx_hardcaml

opam install -y core core_unix ppx_jane rope re dune
```

Build and run tests for the day you wish to target (or all days):

- `dune build`

- `dune runtest test/dX/`

To export to VHDL:


## Implementation notes

### Day 1

Todo.

### Day 3

We expect uints from 0-9 on `data_in` (and `data_valid` high), followed by a single cycle with `data_sep` high (and `data_valid` low). Inputs can be every clock cycle (ie. no cycle needed with `data_sep` low needed between sequential `data_in`s). After the `finish` channel is input, the `sum` output will show the total sum, and the `valid` output will go high (max 2 cycles after `finish` is set high). 

Switch between parts by setting `part` low (part 1) or high (part 2). [TODO!]

### Day 7

For this one I decided not to preprocess the input at all, so we expect characters on `data_in`, exactly as in the problem input. 