module P = Probes_lib

let boo () =
  P.Self.update ~force:true (P.All P.Enable);
  [%probe "boo" (Printf.printf "BOO!\n%!")];
  let boo_enabled = [%probe_is_enabled "boo"] in
  if boo_enabled then Printf.printf "BOO was enabled - first top level%!\n"
;;

let () =
  match Unix.fork () with
  | 0 ->
    (* Child *)
    Unix.sleep 1;
    boo ();
    exit 0
  | _ ->
    Unix.sleep 1;
    exit 0
;;
