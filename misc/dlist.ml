module DoublyList = struct
  type 'a t = 'a list * 'a option * 'a list

  let empty = ([], None, [])

  let of_list = function
    | [] -> [], None, []
    | x :: xs -> [], Some x, xs

  let current = function
    | _, x, _ -> x

  let next = function
    | front, Some x, y :: rear ->
       x :: front, Some y, rear
    | _ -> [], None, []

  let prev = function
    | y :: front, Some x, rear ->
       front, Some y, x :: rear
    | _ -> [], None, []

  let cons x = function
    | front, Some y, rear ->
       front, Some x, y :: rear
    | front, None, rear ->
       front, Some x, rear

  let front = function
    | [], x, _ -> x
    | front, _, _  -> Some (List.hd (List.rev front))

  let rear = function
    | _, x, [] -> x
    | _, _, rear    -> Some (List.hd (List.rev rear))

  let pop_rear = function
    | _, None, _ -> [], None, []
    | [], Some _, [] -> [], None, []
    | x :: front, Some _, [] -> front, Some x, []
    | front, Some x, rear -> front, Some x, List.(rev (tl (rev rear)))

  let rec map f = function
    | (front, Some x, rear) ->
       (List.map f front, Some (f x), List.map f rear)
    | empty -> empty
end

module DList = DoublyList
