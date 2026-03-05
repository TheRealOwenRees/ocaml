(* {{name}} - {{version}} *)
open Base
open OUnit2
open Scrabble_score

let ae exp got _test_ctxt = assert_equal ~printer:string_of_int exp got

let tests = [
  {{#cases}}
    "{{description}}" >::
      ae {{#input}}{{expected}} (score ({{number}})){{/input}};
  {{/cases}}
]

let () =
  run_test_tt_main ("scrabble-score tests" >::: tests)