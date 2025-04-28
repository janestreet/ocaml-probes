module P = Probes_lib

let info ~pid ~bpf:_ =
  let prog = P.get_exe pid in
  let t = P.create ~prog ~allow_gigatext:true () in
  let probes = P.get_probe_states t ~pid in
  Array.iter
    (fun (p : P.probe_state) ->
      Printf.printf "%s %s\n" p.name (if p.enabled then "enabled" else "disabled"))
    probes
;;

let attach ~pid ~bpf:_ ~(actions : P.actions) ~force ~allow_gigatext =
  let prog = P.get_exe pid in
  let t = P.create ~prog ~allow_gigatext () in
  P.trace_existing_process t ~pid ~actions ~force
;;

let trace ~prog ~args ~bpf:_ ~(actions : P.actions) ~allow_gigatext =
  let t = P.create ~prog ~allow_gigatext () in
  ignore (P.trace_new_process t ~args ~actions : P.pid)
;;
