let test () =
  [%probe "foo" (Printf.printf "From S: foo\n")];
  Printf.printf "From S: foo was %b\n" [%probe_is_enabled "foo"]
;;
