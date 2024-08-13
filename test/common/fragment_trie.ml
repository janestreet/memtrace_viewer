open! Core
open Memtrace_viewer_common.For_testing

module Location : Fragment_trie.Location with type t = char = struct
  include Char
  module Debug = Char

  let dummy = '_'
end

module Entry = struct
  include Int
  module Debug = Int
end

module Test_tree = struct
  type entry = int
  type location = char

  module Node = struct
    module Id = String

    type t =
      { id : Id.t
      ; entry : entry
      ; incoming_edge : char
      ; children : t list
      ; suffix : t Lazy.t
      }
    [@@deriving fields ~getters]

    let children t = t.children |> List.map ~f:(fun child -> child.incoming_edge, child)

    let suffix t =
      let suffix = t.suffix |> force in
      if phys_equal suffix t then None else Some suffix
    ;;

    let representative t = t

    module Show_id_only = struct
      let sexp_of_t t = Id.sexp_of_t t.id
    end

    module Debug = struct
      type nonrec t = t

      let rec sexp_of_t ({ id; entry; incoming_edge = _; children = _; suffix } as t) =
        [%message
          ""
            ~_:id
            ~_:(entry : int)
            ~suff:(suffix : Show_id_only.t Lazy.t)
            ~children:(children t : (char * t) list)]
      ;;
    end
  end

  type t = { root : Node.t } [@@unboxed]

  let root { root } = root

  module Debug = struct
    type nonrec t = t = { root : Node.Debug.t } [@@unboxed] [@@deriving sexp_of]
  end

  let make words =
    (* Very naive construction for test purposes *)
    let all_letters : Char.Set.t =
      List.concat_map ~f:String.to_list words |> Char.Set.of_list
    in
    let count_occs s =
      (* Assume each letter appears at most once per word (as we do for traces) *)
      List.count words ~f:(fun word -> String.is_substring word ~substring:s)
    in
    let nodes : Node.t Node.Id.Table.t = Node.Id.Table.create () in
    let rec mk_node ~id ~incoming_edge ~remaining : Node.t option =
      let entry = count_occs id in
      if entry = 0
      then None
      else (
        let children = mk_children ~parent_id:id ~remaining in
        let suffix_id = String.subo id ~pos:1 in
        let suffix = lazy (Hashtbl.find_exn nodes suffix_id) in
        let node : Node.t = { id; entry; incoming_edge; children; suffix } in
        Hashtbl.add_exn nodes ~key:id ~data:node;
        Some node)
    and mk_children ~parent_id ~remaining =
      Set.fold_right remaining ~init:[] ~f:(fun edge children ->
        let id = parent_id ^ (edge |> String.of_char) in
        let remaining = Set.remove remaining edge in
        match mk_node ~id ~incoming_edge:edge ~remaining with
        | Some child -> child :: children
        | None -> children)
    in
    let total_length = List.sum (module Int) ~f:String.length words in
    let children_of_root = mk_children ~parent_id:"" ~remaining:all_letters in
    let rec root : Node.t =
      { id = ""
      ; entry = total_length
      ; incoming_edge = Location.dummy
      ; children = children_of_root
      ; suffix = lazy root
      }
    in
    Hashtbl.add_exn nodes ~key:"" ~data:root;
    { root }
  ;;
end

module _ : Fragment_trie.Suffix_tree = Test_tree
module Test_fragment_trie = Fragment_trie.Make (Location) (Entry) (Unit)

let traverse_in_parallel ~fragment_trie ~(suffix_tree : Test_tree.t) ~f =
  let rec traverse ~fragment ~(suffix_tree_node : Test_tree.Node.t) =
    f ~fragment ~suffix_tree_node;
    List.iter suffix_tree_node.children ~f:(fun suffix_tree_child ->
      let edge = suffix_tree_child.incoming_edge in
      let extended_fragment =
        Test_fragment_trie.Fragment.extend fragment ~orient:Callees edge
        |> Option.value_exn
      in
      traverse ~fragment:extended_fragment ~suffix_tree_node:suffix_tree_child)
  in
  let empty_fragment = Test_fragment_trie.Trie.empty_fragment fragment_trie in
  let suffix_root = suffix_tree.root in
  traverse ~fragment:empty_fragment ~suffix_tree_node:suffix_root
;;

let check_fragment_trie fragment_trie ~suffix_tree =
  let module Table = Test_tree.Node.Id.Table in
  let fragments_by_suffix_tree_id : Test_fragment_trie.Fragment.t Table.t =
    let table = Table.create () in
    traverse_in_parallel
      ~fragment_trie
      ~suffix_tree
      ~f:(fun ~fragment ~suffix_tree_node ->
        Hashtbl.add_exn table ~key:suffix_tree_node.id ~data:fragment);
    table
  in
  traverse_in_parallel ~fragment_trie ~suffix_tree ~f:(fun ~fragment ~suffix_tree_node ->
    let suffix_node = suffix_tree_node.suffix |> force in
    let expected_retracted_fragment =
      Hashtbl.find_exn fragments_by_suffix_tree_id suffix_node.id
    in
    match Test_fragment_trie.Fragment.retract fragment ~orient:Callers with
    | None ->
      (* Must be root *)
      assert (phys_equal suffix_tree_node suffix_node)
    | Some actual_retracted_fragment ->
      assert (
        Test_fragment_trie.Fragment.same
          expected_retracted_fragment
          actual_retracted_fragment));
  ()
;;

let test_tree1 =
  Test_tree.make [ "a"; "ab"; "ab"; "abc"; "ac"; "b"; "ba"; "ba"; "bac"; "d" ]
;;

let test_suffix_tree suffix_tree =
  print_s [%message (suffix_tree : Test_tree.Debug.t)];
  let fragment_trie =
    Test_fragment_trie.Trie.of_suffix_tree (module Test_tree) test_tree1 ~metadata:()
  in
  print_s [%message (fragment_trie : Test_fragment_trie.For_testing.Dumped.t)];
  check_fragment_trie fragment_trie ~suffix_tree
;;

let%expect_test "test tree 1" =
  test_suffix_tree test_tree1;
  (* Note that the ids in the fragment trie can change, so long as they do so
     consistently. *)
  [%expect
    {|
    (suffix_tree
     ((root
       ("" 19 (suff "")
        (children
         ((a
           (a 8 (suff "")
            (children
             ((b
               (ab 3 (suff b) (children ((c (abc 1 (suff bc) (children ())))))))
              (c (ac 2 (suff c) (children ())))))))
          (b
           (b 7 (suff "")
            (children
             ((a
               (ba 3 (suff a) (children ((c (bac 1 (suff ac) (children ())))))))
              (c (bc 1 (suff c) (children ())))))))
          (c (c 3 (suff "") (children ()))) (d (d 1 (suff "") (children ())))))))))
    (fragment_trie
     ((root
       ((id 0) (entry 19) (first_caller _) (last_callee _)
        (retraction_id_by_caller 0)
        (extension_ids_by_caller ((a 1) (b 5) (c 7) (d 10)))
        (extensions_by_callee
         ((a
           ((id 1) (entry 8) (first_caller a) (last_callee a)
            (retraction_id_by_caller 0) (extension_ids_by_caller ((b 8)))
            (extensions_by_callee
             ((b
               ((id 2) (entry 3) (first_caller a) (last_callee b)
                (retraction_id_by_caller 5) (extension_ids_by_caller ())
                (extensions_by_callee
                 ((c
                   ((id 3) (entry 1) (first_caller a) (last_callee c)
                    (retraction_id_by_caller 4) (extension_ids_by_caller ())
                    (extensions_by_callee ()) (representative_id 3) (length 3)))))
                (representative_id 2) (length 2)))
              (c
               ((id 6) (entry 2) (first_caller a) (last_callee c)
                (retraction_id_by_caller 7) (extension_ids_by_caller ((b 9)))
                (extensions_by_callee ()) (representative_id 6) (length 2)))))
            (representative_id 1) (length 1)))
          (b
           ((id 5) (entry 7) (first_caller b) (last_callee b)
            (retraction_id_by_caller 0) (extension_ids_by_caller ((a 2)))
            (extensions_by_callee
             ((a
               ((id 8) (entry 3) (first_caller b) (last_callee a)
                (retraction_id_by_caller 1) (extension_ids_by_caller ())
                (extensions_by_callee
                 ((c
                   ((id 9) (entry 1) (first_caller b) (last_callee c)
                    (retraction_id_by_caller 6) (extension_ids_by_caller ())
                    (extensions_by_callee ()) (representative_id 9) (length 3)))))
                (representative_id 8) (length 2)))
              (c
               ((id 4) (entry 1) (first_caller b) (last_callee c)
                (retraction_id_by_caller 7) (extension_ids_by_caller ((a 3)))
                (extensions_by_callee ()) (representative_id 4) (length 2)))))
            (representative_id 5) (length 1)))
          (c
           ((id 7) (entry 3) (first_caller c) (last_callee c)
            (retraction_id_by_caller 0) (extension_ids_by_caller ((b 4) (a 6)))
            (extensions_by_callee ()) (representative_id 7) (length 1)))
          (d
           ((id 10) (entry 1) (first_caller d) (last_callee d)
            (retraction_id_by_caller 0) (extension_ids_by_caller ())
            (extensions_by_callee ()) (representative_id 10) (length 1)))))
        (representative_id 0) (length 0)))
      (metadata ())))
    |}]
;;
