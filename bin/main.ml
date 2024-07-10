open Current_incr

let test _ =
  let x = var 0 in
  let y = of_cc (read (of_var x) @@ fun x -> write (x + 1)) in
  Format.printf "%d\n" (observe y);
  change x 3;
  propagate ();
  Format.printf "%d\n" (observe y)

let () = test 0
