let test () =
  [%probe "foo" (Printf.printf "From lib: foo\n")];
  Printf.printf "From lib: foo was %b\n" [%probe_is_enabled "foo"]
;;
