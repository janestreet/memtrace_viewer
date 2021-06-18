open! Core
open Memtrace_viewer_common

(* A generalized suffix tree based on Ukkonen's algorithm
   combined with lossy counting. *)

module type Char = sig
  include Hashable.S_plain
  include Comparable.S_plain with type t := t
  include Sexpable.S with type t := t

  val dummy : t
end

(* Need to do this up top because we can't invoke a generative functor (namely
   [Identifier.Make]) from an applicative one (our own [Make]). *)
module Node_id = struct
  include Identifier.Make ()

  let dummy = first_special
end

module Make (X : Char) = struct
  module Node : sig
    module Id : Identifier.S

    type t

    val id : t -> Id.t
    val label : t -> X.t array

    module Root : sig
      type node = t
      type t

      val create : unit -> t
      val node : t -> node
      val is_node : t -> node -> bool
    end

    val add_leaf : root:Root.t -> parent:t -> array:X.t array -> index:int -> key:X.t -> t
    val split_edge : root:Root.t -> parent:t -> child:t -> len:int -> t
    val set_suffix : root:Root.t -> t -> suffix:t -> unit
    val add_to_count : root:Root.t -> depth:int -> count:int -> t -> unit

    type find_result =
      | Found of t
      | Added of t

    val find_or_add_leaf
      :  root:Root.t
      -> parent:t
      -> array:X.t array
      -> index:int
      -> find_result

    val get_child : root:Root.t -> t -> X.t -> t
    val edge_array : t -> X.t array
    val edge_start : t -> int
    val edge_length : t -> int
    val edge_key : t -> X.t
    val edge_char : t -> int -> X.t
    val has_suffix : t -> bool
    val suffix : t -> t
    val parent : t -> t
    val compress : root:Root.t -> threshold:int -> unit
    val reset_summary : t -> unit
    val update_parents_summary : t -> root:Root.t -> heaviness_threshold:int -> unit
    val finalize_summary : t -> heaviness_threshold:int -> unit
    val iter_children : t -> root:Root.t -> f:(t -> unit) -> unit
    val fold_children : t -> root:Root.t -> init:'a -> f:(t -> 'a -> 'a) -> 'a
    val is_heavy : t -> heaviness_threshold:int -> bool
    val contains_heavy : t -> heaviness_threshold:int -> bool
    val total_count : t -> int
    val light_count : t -> int
    val representative : t -> t

    module Debug : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    module Debug_full : sig
      type nonrec t = t [@@deriving sexp_of]
    end
  end = struct
    module Id = Node_id

    type t =
      { id : Id.t
      ; mutable edge_array : X.t array
      ; mutable edge_start : int
      ; mutable edge_len : int
      ; mutable edge_key : X.t
      ; mutable parent : t
      ; mutable suffix_link : t
      ; mutable next_sibling : t
      ; mutable first_child : t
      ; mutable refcount : int
      (* [2 * incoming suffix links + 2 * has count + chidren]
         A node should be deleted when this is <= 1 *)
      ; mutable summary : summary
      ; mutable queue_item : queue_item
      ; mutable count : int
      ; mutable max_edge_squashed : int
      ; mutable max_child_squashed : int
      }

    and queue_item =
      { node : t
      ; mutable next : queue_item
      ; mutable previous : queue_item
      }

    and summary =
      | No_summary
      | Summary of
          { mutable descendents_count : int
          ; mutable heavy_descendents_count : int
          ; mutable heavy_descendents : Id.Set.t
          ; mutable representative : t
          (* Deepest descendent (possibly itself) with the same set of heavy descendents.
          *)
          }

    let id t = t.id
    let same t1 t2 = phys_equal t1 t2
    let is_removable refcount = refcount = 0
    let is_mergable refcount = refcount = 1
    let is_mergable_or_removable refcount = refcount <= 1
    let dummy_array = [||]

    let rec dummy_queue_item =
      { node = dummy; next = dummy_queue_item; previous = dummy_queue_item }

    and dummy =
      let id = Id.dummy in
      let edge_array = dummy_array in
      let edge_start = 0 in
      let edge_len = 0 in
      let edge_key = X.dummy in
      let refcount = 0 in
      let count = 0 in
      let max_child_squashed = 0 in
      let max_edge_squashed = 0 in
      let summary = No_summary in
      { id
      ; edge_array
      ; edge_start
      ; edge_len
      ; edge_key
      ; parent = dummy
      ; suffix_link = dummy
      ; next_sibling = dummy
      ; first_child = dummy
      ; refcount
      ; summary
      ; queue_item = dummy_queue_item
      ; count
      ; max_edge_squashed
      ; max_child_squashed
      }
    ;;

    let is_dummy t = same t dummy
    let is_real t = not (is_dummy t)

    module Queue = struct
      module Item = struct
        type nonrec t = queue_item

        let root = { node = dummy; next = dummy_queue_item; previous = dummy_queue_item }
        let dummy = dummy_queue_item
        let same t1 t2 = phys_equal t1 t2
        let is_dummy t = same t dummy
        let is_real t = not (is_dummy t)
        let next item = item.next
        let set_next item ~next = item.next <- next
        let set_previous item ~previous = if is_real item then item.previous <- previous
        let fresh ~node ~previous ~next = { node; previous; next }
      end

      type t =
        { mutable fronts : Item.t Array.t
        ; mutable max : int
        }

      let create () =
        let fronts = [||] in
        let max = -1 in
        { fronts; max }
      ;;

      let enlarge t =
        let fronts = t.fronts in
        let old_length = Array.length fronts in
        let new_length = (old_length * 2) + 1 in
        let new_fronts = Array.create ~len:new_length dummy_queue_item in
        Array.blit ~src:fronts ~src_pos:0 ~dst:new_fronts ~dst_pos:0 ~len:old_length;
        t.fronts <- new_fronts
      ;;

      let fresh_front_sentinel () =
        let node = dummy in
        let previous = dummy_queue_item in
        let next = dummy_queue_item in
        Item.fresh ~node ~previous ~next
      ;;

      let add t ~depth ~node =
        if depth > t.max
        then (
          while depth >= Array.length t.fronts do
            enlarge t
          done;
          t.max <- depth);
        let front = t.fronts.(depth) in
        let item =
          if Item.is_real front
          then (
            let previous = front in
            let next = Item.next front in
            let item = Item.fresh ~node ~next ~previous in
            Item.set_previous next ~previous:item;
            Item.set_next previous ~next:item;
            item)
          else (
            let previous = fresh_front_sentinel () in
            t.fronts.(depth) <- previous;
            let next = Item.dummy in
            let item = Item.fresh ~node ~previous ~next in
            Item.set_next previous ~next:item;
            item)
        in
        node.queue_item <- item
      ;;

      let remove ~node ~item =
        let next = item.next in
        let previous = item.previous in
        Item.set_previous next ~previous;
        Item.set_next previous ~next;
        node.queue_item <- Item.dummy
      ;;

      let iter_front front depth f =
        let current = ref (Item.next front) in
        while Item.is_real !current do
          let next_current = !current.next in
          f ~depth !current;
          current := next_current
        done
      ;;

      let iter t f =
        let fronts = t.fronts in
        for i = t.max downto 0 do
          iter_front fronts.(i) i f
        done
      ;;
    end

    let label t =
      let rec loop acc t =
        let edge = Array.sub t.edge_array ~pos:t.edge_start ~len:t.edge_len in
        if not (same t t.parent)
        then loop (edge :: acc) t.parent
        else Array.concat (edge :: acc)
      in
      loop [] t
    ;;

    module Root = struct
      type node = t

      type t =
        { node : node
        ; children : node X.Table.t
        ; queue : Queue.t
        ; id_gen : Id.Generator.t
        }

      let create () =
        let id_gen = Id.Generator.create () in
        let id = Id.Generator.generate id_gen in
        let edge_array = dummy_array in
        let edge_start = 0 in
        let edge_len = 0 in
        let edge_key = X.dummy in
        let next_sibling = dummy in
        let first_child = dummy in
        let refcount = 0 in
        let queue_item = Queue.Item.root in
        let summary = No_summary in
        let count = 0 in
        let max_child_squashed = 0 in
        let max_edge_squashed = 0 in
        let rec node =
          { id
          ; edge_array
          ; edge_start
          ; edge_len
          ; edge_key
          ; parent = node
          ; suffix_link = node
          ; next_sibling
          ; first_child
          ; refcount
          ; summary
          ; queue_item
          ; count
          ; max_edge_squashed
          ; max_child_squashed
          }
        in
        let children = X.Table.create ~size:37 () in
        let queue = Queue.create () in
        { node; children; queue; id_gen }
      ;;

      let node t = t.node
      let is_node t node = same t.node node
      let children t = t.children
      let queue t = t.queue
      let gen_id t = Id.Generator.generate t.id_gen
    end

    let rec set_child_in_list previous current old_child new_child =
      let next = current.next_sibling in
      if same current old_child
      then (
        previous.next_sibling <- new_child;
        new_child.next_sibling <- next)
      else set_child_in_list current next old_child new_child
    ;;

    let set_child ~root ~parent ~key ~old_child ~new_child =
      if Root.is_node root parent
      then X.Table.set (Root.children root) ~key ~data:new_child
      else (
        let first_child = parent.first_child in
        let second_child = first_child.next_sibling in
        if same first_child old_child
        then (
          parent.first_child <- new_child;
          new_child.next_sibling <- second_child)
        else set_child_in_list first_child second_child old_child new_child)
    ;;

    let rec add_child_to_list ~key ~child previous current =
      let current_key = current.edge_key in
      if X.(key < current_key)
      then (
        child.next_sibling <- current;
        previous.next_sibling <- child)
      else add_child_to_list ~key ~child current current.next_sibling
    ;;

    let add_child ~root ~parent ~key ~child =
      if Root.is_node root parent
      then X.Table.add_exn (Root.children root) ~key ~data:child
      else (
        parent.refcount <- parent.refcount + 1;
        let first_child = parent.first_child in
        let first_key = first_child.edge_key in
        if X.(key < first_key)
        then (
          child.next_sibling <- first_child;
          parent.first_child <- child)
        else add_child_to_list ~key ~child first_child first_child.next_sibling)
    ;;

    let rec remove_from_child_list previous current child =
      let next = current.next_sibling in
      if same current child
      then previous.next_sibling <- next
      else remove_from_child_list current next child
    ;;

    let remove_child ~root ~parent ~child =
      if Root.is_node root parent
      then (
        let key = child.edge_key in
        X.Table.remove (Root.children root) key;
        Int.max_value)
      else (
        let first_child = parent.first_child in
        let second_child = first_child.next_sibling in
        let refcount = parent.refcount - 1 in
        parent.refcount <- refcount;
        if same first_child child
        then parent.first_child <- second_child
        else remove_from_child_list first_child second_child child;
        refcount)
    ;;

    let set_suffix ~root t ~suffix =
      t.suffix_link <- suffix;
      if not (Root.is_node root suffix) then suffix.refcount <- suffix.refcount + 2
    ;;

    let remove_incoming ~root t =
      if Root.is_node root t
      then Int.max_value
      else (
        let refcount = t.refcount - 2 in
        t.refcount <- refcount;
        refcount)
    ;;

    let fresh_leaf ~root ~parent ~array ~index ~key =
      let id = Root.gen_id root in
      let edge_array = array in
      let edge_start = index in
      let edge_len = Array.length array - index in
      let edge_key = key in
      let suffix_link = dummy in
      let next_sibling = dummy in
      let first_child = dummy in
      let queue_item = dummy_queue_item in
      let summary = No_summary in
      let count = 0 in
      let refcount = 0 in
      let max_edge_squashed = parent.max_child_squashed in
      let max_child_squashed = parent.max_child_squashed in
      { id
      ; edge_array
      ; edge_start
      ; edge_len
      ; edge_key
      ; parent
      ; suffix_link
      ; next_sibling
      ; first_child
      ; refcount
      ; summary
      ; queue_item
      ; count
      ; max_edge_squashed
      ; max_child_squashed
      }
    ;;

    let add_leaf ~root ~parent ~array ~index ~key =
      let node = fresh_leaf ~root ~parent ~array ~index ~key in
      add_child ~root ~parent ~key ~child:node;
      node
    ;;

    let split_edge ~root ~parent ~child ~len =
      if len = 0
      then parent
      else (
        let edge_array = child.edge_array in
        let edge_start = child.edge_start in
        let edge_key = child.edge_key in
        let child_key = edge_array.(edge_start + len) in
        let new_node =
          let id = Root.gen_id root in
          let edge_len = len in
          let suffix_link = dummy in
          let next_sibling = dummy in
          let refcount = 1 in
          let first_child = child in
          let queue_item = dummy_queue_item in
          let summary = No_summary in
          let count = 0 in
          let max_edge_squashed = child.max_edge_squashed in
          let max_child_squashed = child.max_edge_squashed in
          { id
          ; edge_array
          ; edge_start
          ; edge_len
          ; edge_key
          ; parent
          ; suffix_link
          ; next_sibling
          ; first_child
          ; refcount
          ; summary
          ; queue_item
          ; count
          ; max_edge_squashed
          ; max_child_squashed
          }
        in
        set_child ~root ~parent ~key:edge_key ~old_child:child ~new_child:new_node;
        child.edge_start <- edge_start + len;
        child.edge_len <- child.edge_len - len;
        child.edge_key <- child_key;
        child.parent <- new_node;
        child.next_sibling <- dummy;
        new_node)
    ;;

    let merge_child ~root ~parent t =
      let child = t.first_child in
      let key = t.edge_key in
      let edge_len = t.edge_len in
      let child_edge_start = child.edge_start in
      child.edge_key <- key;
      if child_edge_start >= edge_len
      then child.edge_start <- child_edge_start - edge_len
      else (
        let edge_array = t.edge_array in
        let edge_start = t.edge_start in
        let common_prefix = edge_start + (edge_len - child_edge_start) in
        let common =
          if Array.length edge_array = common_prefix
          then edge_array
          else Array.sub edge_array ~pos:0 ~len:common_prefix
        in
        let array = Array.append common child.edge_array in
        child.edge_array <- array;
        child.edge_start <- t.edge_start);
      child.edge_len <- edge_len + child.edge_len;
      let max_edge_squashed = t.max_edge_squashed in
      let child_max_edge_squashed = child.max_edge_squashed in
      if max_edge_squashed > child_max_edge_squashed
      then child.max_edge_squashed <- max_edge_squashed;
      child.parent <- parent;
      set_child ~root ~parent ~key ~old_child:t ~new_child:child
    ;;

    type find_result =
      | Found of t
      | Added of t

    let rec find_or_add_leaf_in_list ~root ~parent ~array ~index ~key previous current =
      let current_key = current.edge_key in
      if X.(key < current_key)
      then (
        let child = fresh_leaf ~root ~parent ~array ~index ~key in
        child.next_sibling <- current;
        previous.next_sibling <- child;
        parent.refcount <- parent.refcount + 1;
        Added child)
      else if X.equal key current_key
      then Found current
      else
        find_or_add_leaf_in_list
          ~root
          ~parent
          ~array
          ~index
          ~key
          current
          current.next_sibling
    ;;

    let find_or_add_leaf ~root ~parent ~array ~index =
      let key = array.(index) in
      if Root.is_node root parent
      then (
        let children = Root.children root in
        match X.Table.find children key with
        | Some child -> Found child
        | None ->
          let leaf = fresh_leaf ~root ~parent ~array ~index ~key in
          X.Table.add_exn children ~key ~data:leaf;
          Added leaf)
      else (
        let first_child = parent.first_child in
        let first_key = first_child.edge_key in
        if X.(key < first_key)
        then (
          let leaf = fresh_leaf ~root ~parent ~array ~index ~key in
          leaf.next_sibling <- first_child;
          parent.first_child <- leaf;
          parent.refcount <- parent.refcount + 1;
          Added leaf)
        else if X.equal key first_key
        then Found first_child
        else
          find_or_add_leaf_in_list
            ~root
            ~parent
            ~array
            ~index
            ~key
            first_child
            first_child.next_sibling)
    ;;

    let rec get_child_in_list current char =
      if X.equal current.edge_key char
      then current
      else if is_dummy current
      then failwith "get_child_in_list: No such child"
      else get_child_in_list current.next_sibling char
    ;;

    let get_child ~root t char =
      if Root.is_node root t
      then (
        match X.Table.find (Root.children root) char with
        | Some child -> child
        | None -> failwith "get_child: No such child")
      else get_child_in_list t.first_child char
    ;;

    let edge_array t = t.edge_array
    let edge_start t = t.edge_start
    let edge_length t = t.edge_len
    let edge_key t = t.edge_key
    let edge_char t i = if i = 0 then t.edge_key else t.edge_array.(t.edge_start + i)
    let has_suffix t = is_real t.suffix_link
    let suffix t = t.suffix_link
    let parent t = t.parent
    let count t = t.count

    module Debug = struct
      type nonrec t = t

      let sexp_of_t ({ id; _ } as node) =
        [%message
          (id : Id.t)
            ~total_count:
              (match node.summary with
               | No_summary -> [%sexp "???"]
               | Summary { descendents_count; _ } ->
                 [%sexp (node.count + descendents_count : int)]
                 : Sexp.t)
            ~label:(label node : X.t array)]
      ;;
    end

    module Summary = struct
      type nonrec t = summary =
        | No_summary
        | Summary of
            { mutable descendents_count : int
            ; mutable heavy_descendents_count : int
            ; mutable heavy_descendents : Id.Set.t
            ; mutable representative : t
            }

      let empty representative =
        let descendents_count = 0 in
        let heavy_descendents_count = 0 in
        let heavy_descendents = Id.Set.empty in
        Summary
          { descendents_count
          ; heavy_descendents_count
          ; heavy_descendents
          ; representative
          }
      ;;

      let descendents_count t =
        match t with
        | No_summary -> failwith "No summary"
        | Summary { descendents_count; _ } -> descendents_count
      ;;

      let heavy_descendents_count t =
        match t with
        | No_summary -> failwith "No summary"
        | Summary { heavy_descendents_count; _ } -> heavy_descendents_count
      ;;

      let heavy_descendents t =
        match t with
        | No_summary -> failwith "No summary"
        | Summary { heavy_descendents; _ } -> heavy_descendents
      ;;

      let representative t =
        match t with
        | No_summary -> failwith "No summary"
        | Summary { representative; _ } -> representative
      ;;

      let add_grand_child t ~grand_child_total_count ~grand_child_heavy_count =
        match t with
        | No_summary -> failwith "No summary"
        | Summary s ->
          s.descendents_count <- s.descendents_count - grand_child_total_count;
          s.heavy_descendents_count <- s.heavy_descendents_count - grand_child_heavy_count
      ;;

      let add_child t ~child ~child_total_count ~child_heavy_count =
        match t with
        | No_summary -> failwith "No summary"
        | Summary s ->
          (match child.summary with
           | No_summary -> failwith "No summary"
           | Summary { heavy_descendents; representative; _ } ->
             s.descendents_count <- s.descendents_count + child_total_count;
             s.heavy_descendents_count <- s.heavy_descendents_count + child_heavy_count;
             s.heavy_descendents <- Set.union s.heavy_descendents heavy_descendents;
             if Set.length heavy_descendents = Set.length s.heavy_descendents
             then s.representative <- representative)
      ;;

      let add_heavy_descendent t ~node =
        match t with
        | No_summary -> failwith "No summary"
        | Summary s -> s.heavy_descendents <- Set.add s.heavy_descendents node.id
      ;;

      let finalize_representative t ~node =
        match t with
        | No_summary -> failwith "No summary"
        | Summary s ->
          let node_length = Set.length s.heavy_descendents in
          let rep_descendents = heavy_descendents s.representative.summary in
          let rep_length = Set.length rep_descendents in
          if node_length > rep_length
          then s.representative <- node
          else (
            assert (node_length = rep_length);
            s.heavy_descendents <- rep_descendents)
      ;;
    end

    let reset_summary t = t.summary <- Summary.empty t
    let descendents_count t = Summary.descendents_count t.summary
    let heavy_descendents_count t = Summary.heavy_descendents_count t.summary
    let heavy_descendents t = Summary.heavy_descendents t.summary
    let representative t = Summary.representative t.summary
    let total_count t = count t + descendents_count t
    let light_count t = total_count t - heavy_descendents_count t

    let is_heavy t ~heaviness_threshold =
      let light_count = light_count t in
      let delta = t.max_edge_squashed in
      light_count + delta > heaviness_threshold
    ;;

    let heavy_count t ~heaviness_threshold =
      if is_heavy t ~heaviness_threshold then total_count t else heavy_descendents_count t
    ;;

    let contains_heavy t ~heaviness_threshold =
      let total_count = total_count t in
      let delta = t.max_edge_squashed in
      total_count + delta > heaviness_threshold
    ;;

    module Debug_full = struct
      type nonrec t = t

      let rec sexp_of_t
                ({ id
                 ; count
                 ; max_edge_squashed
                 ; edge_array
                 ; edge_start
                 ; edge_len
                 ; parent
                 ; suffix_link
                 ; summary
                 ; _
                 } as node)
        =
        let summary_data =
          match summary with
          | No_summary -> [%sexp "No_summary"]
          | Summary _ ->
            [%message
              ""
                ~total_count:(total_count node : int)
                ~light_count:(light_count node : int)
                ~max_light_count:(light_count node + max_edge_squashed : int)
                ~heavy_descendents:(heavy_descendents node : Id.Set.t)]
        in
        let representative =
          match summary with
          | No_summary -> [%sexp "<no summary>"]
          | Summary _ ->
            if same node (representative node)
            then [%sexp "<self>"]
            else [%sexp (representative node : t)]
        in
        [%message
          (id : Id.t)
            ~label:(label node : X.t array)
            (count : int)
            (max_edge_squashed : int)
            (summary_data : Sexp.t)
            ~edge:(Array.sub edge_array ~pos:edge_start ~len:edge_len : X.t array)
            (parent.id : Id.t)
            (suffix_link.id : Id.t)
            (representative : Sexp.t)]
      ;;
    end

    let update_parents_summary t ~root ~heaviness_threshold =
      if not (Root.is_node root t)
      then (
        let total_count = total_count t in
        let heavy_count = heavy_count t ~heaviness_threshold in
        let parent = t.parent in
        let suffix = t.suffix_link in
        let grand_parent = t.parent.suffix_link in
        if is_real parent
        then
          Summary.add_child
            parent.summary
            ~child:t
            ~child_total_count:total_count
            ~child_heavy_count:heavy_count;
        if is_real suffix
        then
          Summary.add_child
            suffix.summary
            ~child:t
            ~child_total_count:total_count
            ~child_heavy_count:heavy_count;
        if is_real grand_parent
        then
          Summary.add_grand_child
            grand_parent.summary
            ~grand_child_total_count:total_count
            ~grand_child_heavy_count:heavy_count)
    ;;

    let finalize_summary t ~heaviness_threshold =
      if is_heavy t ~heaviness_threshold
      then Summary.add_heavy_descendent t.summary ~node:t;
      Summary.finalize_representative t.summary ~node:t
    ;;

    let iter_over_child_list f current =
      let current = ref current in
      while not (phys_equal !current dummy) do
        let child = !current in
        f child;
        current := child.next_sibling
      done
    ;;

    let iter_children t ~root ~f =
      if Root.is_node root t
      then X.Table.iter ~f (Root.children root)
      else iter_over_child_list f t.first_child
    ;;

    let fold_over_child_list ~f current acc =
      let acc = ref acc in
      let current = ref current in
      while not (phys_equal !current dummy) do
        let child = !current in
        acc := f child !acc;
        current := child.next_sibling
      done;
      !acc
    ;;

    let fold_children t ~root ~init ~f =
      if Root.is_node root t
      then X.Table.fold ~f:(fun ~key:_ ~data acc -> f data acc) (Root.children root) ~init
      else fold_over_child_list ~f t.first_child init
    ;;

    let add_to_count t ~count = t.count <- t.count + count

    let register_for_compression ~queue ~depth t =
      if Queue.Item.is_dummy t.queue_item then Queue.add queue ~depth ~node:t
    ;;

    let add_squashed_child t ~upper_bound =
      if upper_bound > t.max_child_squashed then t.max_child_squashed <- upper_bound
    ;;

    let add_squashed_edge t ~upper_bound =
      if upper_bound > t.max_edge_squashed then t.max_edge_squashed <- upper_bound
    ;;

    let rec squash ~root ~queue ~threshold ~depth ~count ~upper_bound ~refcount t =
      t.count <- 0;
      let parent = t.parent in
      let suffix = t.suffix_link in
      let grand_parent = t.parent.suffix_link in
      add_squashed_edge t ~upper_bound;
      add_squashed_child parent ~upper_bound;
      let parent_depth = depth - t.edge_len in
      let suffix_depth = depth - 1 in
      let grand_parent_count = 0 - count in
      add_to_count grand_parent ~count:grand_parent_count;
      add_to_count parent ~count;
      add_to_count suffix ~count;
      if is_removable refcount
      then (
        let parent_refcount = remove_child ~root ~parent ~child:t in
        if is_mergable_or_removable parent_refcount
        then register_for_compression ~queue ~depth:parent_depth parent)
      else if is_mergable refcount
      then merge_child ~root ~parent t;
      if is_mergable_or_removable refcount
      then (
        let suffix_refcount = remove_incoming ~root suffix in
        if is_removable suffix_refcount
        then (
          let count = suffix.count in
          let delta = suffix.max_edge_squashed in
          let upper_bound = count + delta in
          if upper_bound < threshold
          then
            squash
              ~root
              ~queue
              ~threshold
              ~depth:suffix_depth
              ~count
              ~upper_bound
              ~refcount:suffix_refcount
              suffix
          else register_for_compression ~queue ~depth:suffix_depth suffix)
        else if is_mergable suffix_refcount
        then register_for_compression ~queue ~depth:suffix_depth suffix)
    ;;

    let maybe_squash_item ~root ~queue ~threshold ~depth item =
      let node = item.node in
      let count = node.count in
      let delta = node.max_edge_squashed in
      let upper_bound = count + delta in
      if upper_bound < threshold
      then (
        Queue.remove ~node ~item;
        let refcount = node.refcount in
        squash ~root ~queue ~threshold ~depth ~count ~upper_bound ~refcount node)
    ;;

    let compress ~root ~threshold =
      let queue = Root.queue root in
      Queue.iter queue (maybe_squash_item ~root ~queue ~threshold)
    ;;

    let add_to_count ~root ~depth ~count t =
      let queue = Root.queue root in
      add_to_count ~count t;
      register_for_compression ~queue ~depth t
    ;;
  end

  module Cursor : sig
    type t

    val create : at:Node.t -> t
    val goto : t -> Node.t -> unit
    val retract : t -> distance:int -> unit

    type find_result =
      | Found
      | Added of
          { parent : Node.t
          ; leaf : Node.t
          }

    val find_or_add_leaf
      :  root:Node.Root.t
      -> t
      -> array:X.t array
      -> index:int
      -> find_result

    val split_at : root:Node.Root.t -> t -> Node.t
    val goto_suffix : root:Node.Root.t -> t -> Node.t -> unit
  end = struct
    type t =
      { mutable parent : Node.t
      ; mutable len : int
      ; mutable child : Node.t
      }

    (* = parent if len is 0 *)

    let create ~at = { parent = at; len = 0; child = at }

    let goto t node =
      t.parent <- node;
      t.len <- 0;
      t.child <- node
    ;;

    let rec retract t ~distance =
      let len = t.len in
      if len > distance
      then t.len <- len - distance
      else if len = distance
      then (
        t.len <- 0;
        t.child <- t.parent)
      else (
        let distance = distance - len in
        let parent = t.parent in
        t.child <- parent;
        t.parent <- Node.parent parent;
        t.len <- Node.edge_length parent;
        retract t ~distance)
    ;;

    (* Move cursor 1 character towards child. Child assumed
       not equal to parent. *)
    let extend t =
      let len = t.len + 1 in
      if Node.edge_length t.child <= len
      then (
        t.parent <- t.child;
        t.len <- 0)
      else t.len <- len
    ;;

    (* Try to move cursor n character towards child. Child assumed
       not equal to parent. Returns number of characters actually moved.
       Guaranteed to move at least 1 character. *)
    let extend_n t n =
      let len = t.len in
      let target = len + n in
      let edge_len = Node.edge_length t.child in
      if edge_len <= target
      then (
        t.parent <- t.child;
        t.len <- 0;
        edge_len - len)
      else (
        t.len <- target;
        n)
    ;;

    type find_result =
      | Found
      | Added of
          { parent : Node.t
          ; leaf : Node.t
          }

    let find_or_add_leaf ~root t ~array ~index =
      let len = t.len in
      let parent = t.parent in
      if len = 0
      then (
        match Node.find_or_add_leaf ~root ~parent ~array ~index with
        | Found child ->
          t.child <- child;
          extend t;
          Found
        | Added leaf -> Added { parent; leaf })
      else (
        let char = array.(index) in
        let next_char = Node.edge_char t.child len in
        if X.equal char next_char
        then (
          extend t;
          Found)
        else (
          let child = t.child in
          let parent = Node.split_edge ~root ~parent ~child ~len in
          let leaf = Node.add_leaf ~root ~parent ~array ~index ~key:char in
          goto t parent;
          Added { parent; leaf }))
    ;;

    let split_at ~root t =
      let len = t.len in
      if len = 0
      then t.parent
      else (
        let node = Node.split_edge ~root ~parent:t.parent ~child:t.child ~len in
        goto t node;
        node)
    ;;

    let rec rescan ~root t ~array ~start ~len =
      if len <> 0
      then (
        if t.len = 0
        then (
          let char = array.(start) in
          let child = Node.get_child ~root t.parent char in
          t.child <- child);
        let diff = extend_n t len in
        let start = start + diff in
        let len = len - diff in
        rescan ~root t ~array ~start ~len)
    ;;

    let rescan1 ~root t ~key =
      if t.len = 0
      then (
        let child = Node.get_child ~root t.parent key in
        t.child <- child);
      extend t
    ;;

    let rec goto_suffix ~root t node =
      if Node.Root.is_node root node
      then goto t node
      else if Node.has_suffix node
      then goto t (Node.suffix node)
      else (
        let parent = Node.parent node in
        let len = Node.edge_length node in
        if len = 1
        then
          if Node.Root.is_node root parent
          then goto t parent
          else (
            let key = Node.edge_key node in
            goto_suffix ~root t parent;
            rescan1 ~root t ~key)
        else (
          let array = Node.edge_array node in
          let start = Node.edge_start node in
          if Node.Root.is_node root parent
          then (
            goto t parent;
            let start = start + 1 in
            let len = len - 1 in
            rescan ~root t ~array ~start ~len)
          else (
            goto_suffix ~root t parent;
            rescan ~root t ~array ~start ~len)))
    ;;
  end

  type state =
    | Uncompressed
    | Compressed of X.t array

  type t =
    { root : Node.Root.t
    ; mutable max_length : int
    ; mutable count : int
    ; bucket_size : int
    ; mutable current_bucket : int
    ; mutable remaining_in_current_bucket : int
    ; active : Cursor.t
    ; mutable previous_length : int
    ; mutable state : state
    ; mutable heaviness_threshold : int
    }

  let create ~tolerance =
    let root = Node.Root.create () in
    let max_length = 0 in
    let count = 0 in
    let bucket_size = Float.to_int (Float.round_up (1.0 /. tolerance)) in
    let current_bucket = 0 in
    let remaining_in_current_bucket = bucket_size in
    let active = Cursor.create ~at:(Node.Root.node root) in
    let previous_length = 0 in
    let state = Uncompressed in
    let heaviness_threshold = 0 in
    { root
    ; max_length
    ; count
    ; bucket_size
    ; current_bucket
    ; remaining_in_current_bucket
    ; active
    ; previous_length
    ; state
    ; heaviness_threshold
    }
  ;;

  let update_summaries ~heaviness_threshold t =
    let root = t.root in
    let nodes : Node.t list array = Array.create ~len:(t.max_length + 1) [] in
    let rec loop depth node =
      Node.reset_summary node;
      let depth = depth + Node.edge_length node in
      nodes.(depth) <- node :: nodes.(depth);
      Node.iter_children ~root ~f:(loop depth) node
    in
    loop 0 (Node.Root.node root);
    for i = t.max_length downto 0 do
      List.iter nodes.(i) ~f:(fun node ->
        Node.finalize_summary ~heaviness_threshold node;
        Node.update_parents_summary ~root ~heaviness_threshold node)
    done
  ;;

  let rec ensure_suffix ~root cursor t =
    if not (Node.has_suffix t)
    then (
      Cursor.goto_suffix ~root cursor t;
      let suffix = Cursor.split_at ~root cursor in
      ensure_suffix ~root cursor suffix;
      Node.set_suffix ~root t ~suffix)
  ;;

  let insert t ~common_prefix array ~count =
    let len = Array.length array in
    let total_len = common_prefix + len in
    if total_len > t.max_length then t.max_length <- total_len;
    t.count <- t.count + count;
    let root = t.root in
    let active = t.active in
    let array, len, base =
      match t.state with
      | Uncompressed ->
        Cursor.retract active ~distance:(t.previous_length - common_prefix);
        array, len, common_prefix
      | Compressed previous_label ->
        let common = Array.sub previous_label ~pos:0 ~len:common_prefix in
        let array = Array.append common array in
        array, total_len, 0
    in
    let rec loop array len base root active index j =
      if index >= len
      then (
        let destination = Cursor.split_at ~root active in
        ensure_suffix ~root active destination;
        destination)
      else loop_inner array len base root active index j
    and loop_inner array len base root active index j =
      if j > base + index
      then loop array len base root active (index + 1) j
      else (
        match Cursor.find_or_add_leaf ~root active ~array ~index with
        | Found -> loop array len base root active (index + 1) j
        | Added { parent; leaf } ->
          Cursor.goto_suffix ~root active parent;
          let leaf_suffix =
            if Node.has_suffix parent
            then loop_inner array len base root active index (j + 1)
            else (
              let suffix = Cursor.split_at ~root active in
              let leaf_suffix = loop_inner array len base root active index (j + 1) in
              Node.set_suffix ~root parent ~suffix;
              leaf_suffix)
          in
          Node.set_suffix ~root leaf ~suffix:leaf_suffix;
          leaf)
    in
    let destination = loop array len base root active 0 0 in
    Node.add_to_count ~root ~depth:total_len ~count destination;
    let remaining = t.remaining_in_current_bucket - 1 in
    if remaining <= 0
    then (
      t.current_bucket <- t.current_bucket + 1;
      t.remaining_in_current_bucket <- t.bucket_size;
      let destination_label = Node.label destination in
      Cursor.goto active (Node.Root.node t.root);
      let threshold = t.current_bucket in
      Node.compress ~root ~threshold;
      t.previous_length <- 0;
      t.state <- Compressed destination_label)
    else (
      t.remaining_in_current_bucket <- remaining;
      Cursor.goto active destination;
      t.previous_length <- total_len;
      t.state <- Uncompressed)
  ;;

  let root t = t.root

  let threshold_of_frequency t frequency =
    Float.to_int (Float.round_down (frequency *. Float.of_int t.count))
  ;;

  let total_count t = t.count
  let maximum_depth t = t.max_length

  let calculate_totals t ~heaviness_frequency =
    let heaviness_threshold = heaviness_frequency |> threshold_of_frequency t in
    update_summaries ~heaviness_threshold t;
    t.heaviness_threshold <- heaviness_threshold
  ;;

  let is_heavy t node = Node.is_heavy node ~heaviness_threshold:t.heaviness_threshold

  let contains_heavy t node =
    Node.contains_heavy node ~heaviness_threshold:t.heaviness_threshold
  ;;
end
