let () =
  [%probe
    "enabled_at_init"
      ~enabled_at_init:true
      (print_string "enabled_at_init";
       print_newline ())]
;;

let () =
  [%probe
    "disabled_at_init"
      ~enabled_at_init:false
      (print_string "disabled_at_init";
       print_newline ())]
;;

let () =
  let is_native =
    match Sys.backend_type with
    | Native -> true
    | Bytecode | Other _ -> false
  in
  let handler is_native =
    Printf.printf "is_native=%b" is_native;
    print_newline ()
  in
  if is_native
  then [%probe "enabled_at_init_in_bytecode" (handler is_native)]
  else handler is_native
;;
