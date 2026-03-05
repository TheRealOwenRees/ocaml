open OUnit2
open Armstrong_numbers

let ae exp got _test_ctxt = 
  assert_equal ~printer:string_of_bool exp got

let tests = [
  {{#cases}}
    "{{description}}" >::
      ae {{expected}} (armstrong {{#input}}{{number}}{{/input}});
  {{/cases}}
]

let () =
  run_test_tt_main ("armstrong tests" >::: tests)