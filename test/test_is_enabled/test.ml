open Core

let () = [%probe "other" ()]

let test () =
  Probes_lib.Self.update (All Enable) ~force:true;
  print_s [%sexp "enabled?", ([%probe_is_enabled "broken_probe"] : bool)];
  match
    (if [%probe_is_enabled "broken_probe"]
     then (
       print_endline "probe on";
       None)
     else failwith "probe off"
     : Nothing.t option)
  with
  | None -> ()
  | Some nothing ->
    [%probe "broken_probe" ()];
    Nothing.unreachable_code nothing
;;

let%expect_test _ =
  let (_ : _) = Expect_test_helpers_core.show_raise ~hide_positions:true test in
  ();
  [%expect
    {|
    (enabled? true)
    probe on
    "did not raise"
    |}]
;;
