(* Least Recently Used (LRU) Cache

 A LRU cache is cache whose eviction policy is to evict the least
 recently used elements.  *)

module type LRU_CACHE = sig
  type 'a t
      (* the cache contains items of type 'a *)

  val capacity : 'a t -> int
      (* positive integer *)
  val get : 'a t -> int -> 'a
      (* O(1) read operation *)
  val put : 'a t -> 'a -> int
      (* O(1) write operation *)
end

module DList = struct
  type 'a t =
    | Nil
    | Cons of 'a cons_cell
  and 'a cons_cell =
    { mutable prev: 'a cons_cell option;
      mutable data: 'a;
      mutable next: 'a cons_cell option; }

  let nil = Nil

  let singleton x =
    Cons { prev = None;
           data = x;
           next = None }

  let cons x = function
    | Nil -> singleton x
    | Cons xs ->
       match xs.prev with
       | None ->
          let cell =
            { prev = None;
              data = x;
              next = Some xs }
          in
          xs.prev <- Some cell;
          Cons cell
       | Some ys ->
          let cell =
            { prev = Some ys;
              data = x;
              next = Some xs }
          in
          ys.next <- Some cell;
          Cons cell

  let detach = function
    | Nil -> Nil
    | Cons xs ->
       match xs.prev, xs.next with
       | None, None ->
          singleton xs.data
       | Some ys, None ->
          ys.next <- None;
          singleton xs.data
       | None, Some ys ->
          ys.prev <- None;
          singleton xs.data
       | Some ys, Some zs ->
          ys.next <- Some zs;
          zs.prev <- Some ys;
          singleton xs.data

  let attach xs ys =
    match xs, ys with
    | Nil, Nil -> Nil
    | xs, Nil -> xs
    | Nil, ys -> ys
    | Cons xs, Cons ys ->
       xs.next <- Some ys;
       ys.prev <- Some xs;
       Cons xs

  let rec iter f = function
    | Nil -> ()
    | Cons { next; data; _ } ->
       f data;
       match next with
       | None -> ()
       | Some xs -> iter f (Cons xs)

  let next = function
    | Nil -> assert false
    | Cons { next; _ } ->
       match next with
       | None -> assert false
       | Some cell -> Cons cell

  let prev = function
    | Nil -> assert false
    | Cons { prev; _ } ->
       match prev with
       | None -> assert false
       | Some cell -> Cons cell
end


module LRU_Cache = struct
  type 'a t = {
      capacity: int;
      store: (int, 'a * (int DList.t)) Hashtbl.t;
      mutable lru: int DList.t; (* Pointer to the least recently used element index *)
      mutable mru: int DList.t;
    }

  let make size =
    assert (size > 0);
    let open DList in
    let store = Hashtbl.create size in
    { capacity = size;
      store;
      lru = nil;
      mru = nil; }

  let get cache i =
    let (x, ptr) = Hashtbl.find cache.store i in
    let ptr = DList.(attach (detach ptr) cache.lru) in
    cache.lru <- ptr;
    x

  let put cache x =
    let open DList in
    if Hashtbl.length cache.store = cache.capacity then
      begin
        match cache.mru with
        | Nil -> assert false
        | Cons cell ->
           let i = cell.data in
           let _ = detach cache.mru in
           cache.mru <- prev (Cons cell);
           let ptr = cons i cache.lru in
           cache.lru <- ptr;
           Hashtbl.remove cache.store i;
           Hashtbl.add cache.store i (x,ptr);
           i
      end
    else
      begin
        let i = Hashtbl.length cache.store in
        let ptr =
          match cache.lru, cache.mru with
          | Nil, Nil ->
             let ptr = DList.singleton i in
             cache.lru <- ptr;
             cache.mru <- ptr;
             ptr
          | _, _ ->
             let ptr = DList.cons i cache.lru in
             cache.lru <- ptr;
             ptr
        in
        Hashtbl.add cache.store i (x,ptr);
        i
      end
end
