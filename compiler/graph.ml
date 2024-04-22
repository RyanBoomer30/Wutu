open Printf
open Errors
module NeighborSet = Set.Make (String)

type neighborst = NeighborSet.t

module Graph = Map.Make (String)

type grapht = neighborst Graph.t

module StringSet = Set.Make (String)

type livet = StringSet.t

let empty : grapht = Graph.empty

let add_node (g : grapht) (name : string) : grapht =
  if Graph.mem name g then
    g
  else
    Graph.add name NeighborSet.empty g
;;

let add_directed_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  let g' = add_node (add_node g n1) n2 in
  let curr_neighbors = Graph.find n1 g' in
  Graph.add n1 (NeighborSet.add n2 curr_neighbors) g'
;;

let add_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  let g' = add_directed_edge g n1 n2 in
  add_directed_edge g' n2 n1
;;

let get_neighbors (g : grapht) (name : string) : string list =
  if Graph.mem name g then
    NeighborSet.fold (fun n ns -> n :: ns) (Graph.find name g) []
  else
    []
;;

let get_vertices (g : grapht) : string list =
  let keys, _ = List.split (Graph.bindings g) in
  keys
;;

let string_of_graph (g : grapht) : string =
  let string_of_neighbors (n : string) : string = ExtString.String.join ", " (get_neighbors g n) in
  ExtString.String.join "\n"
    (List.map (fun k -> sprintf "%s: %s" k (string_of_neighbors k)) (get_vertices g))
;;

let rec all_pairs lst =
  match lst with
  | [] -> []
  | x :: xs ->
      let pairs_with_x = List.map (fun y -> (x, y)) xs in
      pairs_with_x @ all_pairs xs
;;

(* adds edges between all names in the list *)
let add_clique (g : grapht) (names_set : StringSet.t) : grapht =
  let names = StringSet.elements names_set in
  List.fold_left (fun graph_so_far (l, r) -> add_edge graph_so_far l r) g (all_pairs names)
;;

(* removes a node and all of its edges from the graph *)
let remove_node (g : grapht) (name : string) : grapht =
  if Graph.mem name g then
    let g_no = Graph.remove name g in
    Graph.map (fun neighbors -> NeighborSet.remove name neighbors) g_no
  else
    g
;;

let smallest_degree_node (g : grapht) : string =
  let vertices = get_vertices g in
  let degrees = List.map (fun v -> (v, List.length (get_neighbors g v))) vertices in
  let sorted = List.sort (fun (_, d1) (_, d2) -> compare d1 d2) degrees in
  match sorted with
  | [] -> raise (InternalCompilerError "empty graph")
  | (v, _) :: _ -> v
;;

let highest_degree_num (g : grapht) : int =
  let vertices = get_vertices g in
  let degrees = List.map (fun v -> List.length (get_neighbors g v)) vertices in
  let sorted = List.sort (fun d1 d2 -> compare d2 d1) degrees in
  match sorted with
  | [] -> raise (InternalCompilerError "empty_graph")
  | x :: _ -> x
;;

(* generates $K_n$, with nodes labeled by numbers (as strings) *)
let complete_graph n : grapht =
  let names = List.init n string_of_int in
  add_clique empty (StringSet.of_list names)
;;
