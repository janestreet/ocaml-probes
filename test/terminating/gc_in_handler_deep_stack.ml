(* Example with a probe that allocates and calls a gc *)

let rec h x y z n =
  Printf.printf "h: %d %f %s\n" x y z;
  let len = if x < 0 || x > 0xFFFF_FFFF then 10000 else 0 in
  let arr = Array.make len y in
  let total = Array.fold_right (fun v acc -> v +. acc) arr 0. in
  if n = 0 then Gc.full_major () else h x y z (n - 1);
  Printf.printf "total = %f\n" total;
  ()
;;

let foo a b c =
  [%probe "fooia" (h a b c 15)];
  a, b, Float.of_int a +. b
;;

let rec fib i j =
  if i >= 0
  then (
    if i mod 2 = 0
    then (
      let a, b, c = foo (i + j) Float.(of_int i /. of_int j) "myau" in
      Printf.printf "after foo %d %f %f\n" a b c);
    fib j (i + j))
;;

let () = fib 0 1
