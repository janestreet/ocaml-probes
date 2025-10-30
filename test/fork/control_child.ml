module P = Probes_lib

let boo () =
  [%probe "boo" (Printf.printf "BOO!\n")];
  let boo_enabled = [%probe_is_enabled "boo"] in
  if boo_enabled then Printf.printf "BOO was enabled - first top level\n"
;;

let () =
  let prev = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigusr1 ] in
  match Unix.fork () with
  | 0 ->
    (* Child *)
    let ready = Atomic.make false in
    Sys.Safe.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> Atomic.set ready true));
    while not (Atomic.get ready) do
      Unix.sigsuspend prev;
      let cur = Unix.sigprocmask Unix.SIG_SETMASK prev in
      let _ : int list = Unix.sigprocmask Unix.SIG_SETMASK cur in
      ()
    done;
    boo ();
    exit 0
  | pid ->
    (* Parent *)
    let t = P.create ~prog:Sys.executable_name () in
    P.trace_existing_process t ~atomically:true ~actions:(P.All P.Enable) ~pid;
    Unix.kill pid Sys.sigusr1;
    let _, _ = Unix.waitpid [] pid in
    [%probe "fail" (Printf.printf "FAIL!\n")];
    if [%probe_is_enabled "boo"] then print_endline "FAIL!";
    exit 0
;;
