let () =
  while true do
    [%probe "prog2" (print_endline "from prog2 probe")];
    print_endline "from prog2"
  done
;;
