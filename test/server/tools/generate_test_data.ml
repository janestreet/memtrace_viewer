open! Core

let n = 50
let empty_array = [||]

let[@cold] make_array () : int array array =
  let array = Array.create ~len:n empty_array in
  array
;;

(* Roll a 100-sided die and return the result. (Using integers instead of floats for this
   because they don't have to be allocated.) *)
let roll () = Random.bits () mod 100

let[@cold] rec a array i =
  while roll () < 50 && !i < n do
    let () =
      if roll () < 10
      then (
        array.(!i) <- [| 0; 1; 2; 3; 4; 5 |];
        if roll () < 50 then incr i)
    in
    match roll () with
    | r when r < 50 -> b array i
    | r when r < 90 -> c array i
    | _ -> ()
  done

and[@cold] b array i =
  while roll () < 50 && !i < n do
    let () =
      if roll () < 5
      then (
        array.(!i) <- [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |];
        if roll () < 40 then incr i)
    in
    match roll () with
    | r when r < 60 -> a array i
    | r when r < 98 -> c array i
    | _ -> ()
  done

and[@cold] c array i =
  while roll () < 50 && !i < n do
    let () =
      if roll () < 1 && roll () < 10
      then (
        array.(!i) <- Array.create ~len:1000 42;
        if roll () < 40 then incr i)
    in
    match roll () with
    | r when r < 30 -> a array i
    | r when r < 99 -> b array i
    | _ -> ()
  done
;;

let go () =
  let array = make_array () in
  let i = ref 0 in
  while !i < n do
    match roll () with
    | r when r < 33 -> a array i
    | r when r < 67 -> b array i
    | _ -> c array i
  done;
  Gc.major ();
  for i = 0 to (n / 2) - 1 do
    array.(i * 2) <- empty_array
  done;
  Gc.major ();
  i := 0;
  while !i < n / 5 do
    match roll () with
    | r when r < 33 -> a array i
    | r when r < 67 -> b array i
    | _ -> c array i
  done
;;

let main ~filename =
  Gc.set { (Gc.get ()) with minor_heap_size = 1000 };
  Format.eprintf "%d\n" (Gc.get ()).minor_heap_size;
  Random.init 0;
  let _tracer = Memtrace.start_tracing ~filename ~sampling_rate:1e-1 ~context:None in
  go ();
  ()
;;

let command =
  Command.basic
    ~summary:"Generate some test data for Memtrace"
    (let%map_open.Command filename = anon ("FILENAME" %: string) in
     fun () -> main ~filename)
;;

let () = Command_unix.run command
