(* Create more than one tracee and update their probes. *)
module T = Probes_lib_test
module P = Probes_lib
module PT = P.With_ptrace

type t =
  { prog : string
  ; pt : P.t
  ; pid : P.pid
  ; names : P.probe_name array
  }

let trace prog actions =
  let pt = P.create ~prog ~allow_gigatext:false () in
  let pid = P.trace_new_process pt ~args:[] ~actions in
  let names = P.get_probe_names pt in
  Array.iteri (fun i name -> Printf.printf "%d:%s\n" i name) names;
  { prog; pt; pid; names }
;;

let kill t1 =
  try
    Unix.kill t1.pid Sys.sigkill;
    T.wait t1.pid ~prog:t1.prog
  with
  | Failure s ->
    (* remove pid from s *)
    let spid = string_of_int t1.pid in
    String.split_on_char ' ' s
    |> List.map (fun s -> if String.equal s spid then "<pid>" else s)
    |> String.concat " "
    |> Printf.printf "%s\n";
    (* ignore failure caused by kill *)
    ()
;;

let () =
  let enable_all = P.All P.Enable in
  let disable_all = P.All P.Disable in
  let t1 = trace Sys.argv.(1) enable_all in
  T.print_states (P.get_probe_states ~atomically:false t1.pt ~pid:t1.pid);
  let t2 = trace Sys.argv.(2) disable_all in
  T.print_states (P.get_probe_states ~atomically:true t2.pt ~pid:t2.pid);
  (* Attach only to t1 *)
  PT.attach t1.pt t1.pid;
  (* check that it fails to attach again to the same process to get states *)
  (try
     PT.attach t1.pt t1.pid;
     print_endline "attach should have failed"
   with
   | Probes_lib.Error msg
     when String.equal msg (Printf.sprintf "Already attached to %d" t1.pid) ->
     print_endline "Already attached to this process");
  (* check that it fails to attach to a different process without detaching *)
  (try
     PT.attach t1.pt t2.pid;
     print_endline "attach should have failed"
   with
   | Probes_lib.Error msg ->
     let expected_msg =
       Printf.sprintf "Cannot attach to %d, already attached to %d" t2.pid t1.pid
     in
     if String.equal msg expected_msg
     then print_endline "Already attached to another process"
     else print_endline msg);
  (* check that it fails to detach from the process it is not attached to *)
  (try
     PT.detach t2.pt;
     print_endline "detach: should have failed"
   with
   | Probes_lib.Error msg -> print_endline msg);
  (* without detaching, get and update states of t1 using With_ptrace, then get states of
     t1 using both methods. *)
  T.print_states (PT.get_probe_states t1.pt);
  PT.update t1.pt ~actions:disable_all;
  T.print_states (PT.get_probe_states t1.pt);
  PT.update t1.pt ~actions:enable_all;
  T.print_states (PT.get_probe_states t1.pt);
  P.trace_existing_process ~atomically:false t1.pt ~pid:t1.pid ~actions:disable_all;
  T.print_states (P.get_probe_states ~atomically:false t1.pt ~pid:t1.pid);
  P.trace_existing_process ~atomically:true t1.pt ~pid:t1.pid ~actions:enable_all;
  T.print_states (P.get_probe_states ~atomically:true t1.pt ~pid:t1.pid);
  (* get states of t2 using both methods *)
  T.print_states (P.get_probe_states ~atomically:false t2.pt ~pid:t2.pid);
  P.trace_existing_process ~atomically:false t2.pt ~pid:t2.pid ~actions:enable_all;
  T.print_states (P.get_probe_states ~atomically:false t2.pt ~pid:t2.pid);
  P.trace_existing_process ~atomically:true t2.pt ~pid:t2.pid ~actions:disable_all;
  T.print_states (P.get_probe_states ~atomically:true t2.pt ~pid:t2.pid);
  PT.detach t1.pt;
  (* Disable all probes and kill the tracees. *)
  P.trace_existing_process t1.pt ~actions:(P.All P.Disable) ~pid:t1.pid;
  kill t1;
  kill t2;
  ()
;;
