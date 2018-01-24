(* Detect whether a given graph is cyclic *)

module HashSet = struct
  type 'a t = ('a, unit) Hashtbl.t

  let mem tbl k =
    Hashtbl.mem tbl k

  let put tbl k =
    (if mem tbl k then
       Hashtbl.remove tbl k);
    Hashtbl.add tbl k ()

  let get tbl k =
    Hashtbl.find tbl k

  let remove tbl k =
    Hashtbl.remove tbl k

  let size tbl =
    Hashtbl.length tbl

  let make capacity =
    Hashtbl.create capacity
end

module Graph = struct
  (*
     /-------------\
     v             |
     0 --> 1 --> 2 /
            \--> 3
      0 1 2 3
    0 F T F F
    1 F F T T
    2 T F F F
    3 F F F F
   *)
  type t = bool array array

  let make num_vertices edges =
    let graph =
      Array.make_matrix num_vertices num_vertices false
    in
    List.iter
      (fun (x, y) ->
        graph.(x).(y) <- true)
      edges;
    graph

  let outgoing_edges g x =
    Array.fold_left
      (fun acc y ->
        if g.(x).(y)
        then (x,y) :: acc
        else acc)
      [] (Array.mapi (fun i _ -> i) g.(x))

  let iter f g =
    Array.iter
      (fun r ->
        Array.iteri (fun i _ -> f i) r)
      g

  let has_cycle g =
    let exception Break in
    let visited =
      HashSet.make (Array.length g)
    in
    let workq =
      HashSet.make (Array.length g)
    in
    let rec goto i =
      let () = HashSet.put visited i in
      let () = HashSet.put workq i in
      let edges = outgoing_edges g i in
      let res =
        List.fold_left
          (fun acc (_,j) ->
            acc || (HashSet.mem visited j && HashSet.mem workq j) || goto j)
          false edges
      in
      let () = HashSet.remove workq i in
      res
    in
    let rec loop i n acc =
      if not(acc) && i < n
      then
        let acc = acc || goto i in
        loop (i+1) n acc
      else acc
    in
    loop 0 (Array.length g) false

end
