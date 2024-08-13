module P = Probes_lib
module PT = Probes_lib.With_ptrace

let bpf = false

let print_states probes =
  Array.iter
    (fun (p : Probes_lib.probe_state) ->
      Printf.printf "%s %s\n" p.name (if p.enabled then "enabled" else "disabled"))
    probes
;;

let print_info t ~pid =
  PT.attach t pid;
  print_states (PT.get_probe_states t);
  PT.detach t
;;

let attach_test_lib_actions t ~pid ~actions =
  PT.attach t pid;
  PT.update t ~actions;
  PT.detach t
;;

let attach_test_lib t ~pid ~enable =
  let actions = Probes_lib.All (if enable then P.Enable else P.Disable) in
  attach_test_lib_actions t ~pid ~actions
;;

let trace_test_lib ~prog ~args =
  let actions = P.All P.Enable in
  let t = P.create ~prog ~allow_gigatext:false () in
  let pid = PT.start t ~args in
  PT.update t ~actions;
  PT.detach t;
  t, pid
;;

let trace_test_lib_actions ~prog ~args ~actions =
  let t = P.create ~prog ~allow_gigatext:false () in
  let pid = PT.start t ~args in
  (* All probes are disabled initially,
     only enable actions matter at start. *)
  (match actions with
   | P.All P.Disable -> ()
   | P.All P.Enable -> PT.update t ~actions
   | Selected list ->
     let res =
       List.filter
         (fun (action, _) ->
           match action with
           | P.Disable -> false
           | P.Enable -> true)
         list
     in
     PT.update t ~actions:(Selected res));
  PT.detach t;
  t, pid
;;

let wait pid ~prog =
  match Unix.waitpid [] pid with
  | p, WEXITED 0 when p = pid -> ()
  | p, status ->
    let desc, code =
      match status with
      | WEXITED n -> "exited with code", n
      | WSIGNALED n -> "killed with signal", n
      | WSTOPPED n -> "stopped with signal", n
    in
    failwith
      (Printf.sprintf
         "Tracing %s with process id %d failed. Process %d %s %d.\n"
         prog
         pid
         p
         desc
         code)
;;
