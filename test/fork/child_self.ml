module P = Probes_lib

let boo () =
  P.Self.update ~force:true (P.All P.Enable);
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
      (* [Unix.sigsuspend] returns when a signal is delivered but the handler may
         still be pending. Calling [Unix.sigprocmask] guarantees to process
         all pending handlers. There is currently no public interface for
         [caml_process_pending_actions].  *)
      let cur = Unix.sigprocmask Unix.SIG_SETMASK prev in
      let _ : int list = Unix.sigprocmask Unix.SIG_SETMASK cur in
      ()
    done;
    boo ();
    exit 0
  | pid ->
    (* Parent *)
    Unix.kill pid Sys.sigusr1;
    let _, _ = Unix.waitpid [] pid in
    [%probe "fail" (Printf.printf "FAIL!\n")];
    if [%probe_is_enabled "boo"] then print_endline "FAIL!";
    exit 0
;;
