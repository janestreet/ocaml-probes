let () =
  while true do
    [%probe "prog1" (print_endline "from prog1 probe")];
    print_endline "from prog1"
  done
;;
