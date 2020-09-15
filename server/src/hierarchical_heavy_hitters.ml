open! Core_kernel

module Make (X : Hashable.S_plain) = struct
  module Node = struct
    type t =
      { mutable count : int
      ; mutable delta : int
      ; mutable child_delta : int
      ; children : t X.Table.t
      }

    let create () =
      let count = 0 in
      let delta = 0 in
      let child_delta = 0 in
      let children = X.Table.create () in
      { count; delta; child_delta; children }
    ;;

    let find_or_create_child t x =
      match X.Table.find t.children x with
      | Some t -> t
      | None ->
        let count = 0 in
        let delta = t.child_delta in
        let child_delta = delta in
        let children = X.Table.create () in
        let child = { count; delta; child_delta; children } in
        X.Table.add_exn t.children ~key:x ~data:child;
        child
    ;;

    let rec insert t xs count =
      match xs with
      | [] -> t.count <- t.count + count
      | x :: xs -> insert (find_or_create_child t x) xs count
    ;;

    let compress t bucket =
      let rec loop parent t =
        X.Table.filter_map_inplace ~f:(fun child -> loop t child) t.children;
        let empty = X.Table.length t.children = 0 in
        let infrequent = t.count + t.delta <= bucket in
        if empty && infrequent
        then (
          parent.count <- parent.count + t.count;
          parent.child_delta <- max parent.child_delta (t.count + t.delta);
          None)
        else Some t
      in
      X.Table.filter_map_inplace ~f:(fun child -> loop t child) t.children
    ;;

    let output t threshold =
      let compare (_, (count1 : int), _) (_, (count2 : int), _) = compare count2 count1 in
      let rec loop key t =
        let child_count, light_child_count, child_results = loop_children t in
        let results =
          List.map
            ~f:(fun (rest, lower, upper) -> key :: rest, lower, upper)
            child_results
        in
        let count = t.count + child_count in
        if t.count + t.delta + light_child_count > threshold
        then (
          let results = List.merge ~compare [ [ key ], count, count + t.delta ] results in
          count, 0, results)
        else count, t.count + light_child_count, results
      and loop_children t =
        X.Table.fold
          t.children
          ~f:(fun ~key ~data (c, f, r) ->
            let c', f', r' = loop key data in
            c' + c, f' + f, List.merge ~compare r' r)
          ~init:(0, 0, [])
      in
      let _, _, results = loop_children t in
      results
    ;;

    let children t = X.Table.to_alist t.children
    let samples_excluding_children t = t.count
    let delta t = t.delta
  end

  type t =
    { root : Node.t
    ; bucket_size : int
    ; mutable current_bucket : int
    ; mutable remaining : int
    ; mutable total : int
    }

  let create error =
    let root = Node.create () in
    let bucket_size = Float.to_int (Float.round_up (1.0 /. error)) in
    let current_bucket = 0 in
    let remaining = bucket_size in
    let total = 0 in
    { root; bucket_size; current_bucket; remaining; total }
  ;;

  let insert t xs count =
    Node.insert t.root xs count;
    let remaining = t.remaining - 1 in
    if remaining > 0
    then t.remaining <- remaining
    else (
      let current_bucket = t.current_bucket + 1 in
      Node.compress t.root current_bucket;
      t.current_bucket <- current_bucket;
      t.remaining <- t.bucket_size);
    t.total <- t.total + count
  ;;

  let output t frequency =
    let threshold = Float.to_int (Float.round_down (frequency *. Float.of_int t.total)) in
    Node.output t.root threshold
  ;;

  let roots t = Node.children t.root
  let total t = t.total
end
