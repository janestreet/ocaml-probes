let test () =
  [%probe "foo" (Printf.printf "From T: foo\n")];
  Printf.printf "From T: foo was %b\n" [%probe_is_enabled "foo"]
;;
