open Lwt

module Html = Dom_html

let () = Random.self_init ()

let concat ?(sep=" ") lst =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> acc ^ x
    | x :: r -> aux (acc ^ x ^ sep) r
  in aux "" lst

let position_class position =
  "tile-position-"
  ^ (string_of_int @@ Position.y position + 1)
  ^ "-"
  ^ (string_of_int @@ Position.x position + 1)

let value_class tile =
  "tile-" ^ (Tile.to_string tile)

let apply_classes element classes =
  element ## setAttribute
    (Js.string "class",
     Js.string (concat classes))

let safeRemoveChild element node = ()

let add_tile position tile =
  let document = Html.window ## document in
  let wrapper = Html.createDiv document in
  let inner = Html.createDiv document in
  let tileContainer = Js.Opt.case
      (document ## querySelector (Js.string ".tile-container"))
      (fun () -> raise Not_found)
      (fun x -> x) in

  apply_classes wrapper
    ["tile"; "tile-new"; value_class tile; position_class position];
  apply_classes inner ["tile-inner"];
  inner ## innerHTML <- (Js.string @@ Tile.to_string tile);

  Dom.appendChild wrapper inner;
  Dom.appendChild tileContainer wrapper;
  ()

let nth lst idx =
  match Js.Opt.to_option (lst##item(idx)) with
  | Some x -> x
  | None -> raise Not_found

let move_tile p_from p_to tile =
  let document = Html.window ## document in
  let wrapper = Html.createDiv document in
  let inner = Html.createDiv document in

  let tileContainer = Js.Opt.case
      (document ## querySelector (Js.string ".tile-container"))
      (fun () -> raise Not_found)
      (fun x -> x) in
  let tiles =
    (tileContainer ## querySelectorAll
       (Js.string ("." ^ (position_class p_from)))) in

  apply_classes wrapper
    ["tile"; value_class tile; position_class p_to; "hidden"];
  apply_classes inner ["tile-inner"];
  inner ## innerHTML <- (Js.string @@ Tile.to_string tile);

  Lwt_js_events.async (fun () ->
      Lwt_js_events.request_animation_frame () >>= fun () ->

      for i = 0 to (tiles ## length) - 1
      do
        ((nth tiles i) ## classList) ## remove
          (Js.string (position_class p_from));
        ((nth tiles i) ## classList) ## add
          (Js.string (position_class p_to));
      done;

      Lwt.return ()
      >>= (fun () -> Lwt_js_events.transitionend (nth tiles 0))
      >>= (fun () ->
          (wrapper ## classList) ## remove
            (Js.string "hidden");

          for i = 0 to (tiles ## length) - 1
          do
            ((nth tiles i) ## classList) ## add
              (Js.string "hidden");
          done;

          Lwt.return ()));

  Dom.appendChild wrapper inner;
  Dom.appendChild tileContainer wrapper;

  ()

let merge_tile position tile =
  let document = Html.window ## document in
  let wrapper = Html.createDiv document in
  let inner = Html.createDiv document in

  let tileContainer = Js.Opt.case
      (document ## querySelector (Js.string ".tile-container"))
      (fun () -> raise Not_found)
      (fun x -> x) in

  apply_classes wrapper
    ["tile"; "tile-merged"; value_class tile; position_class position];
  apply_classes inner ["tile-inner"];
  inner ## innerHTML <- (Js.string @@ Tile.to_string tile);

  Dom.appendChild wrapper inner;
  Dom.appendChild tileContainer wrapper;

  ()

let actuator = function
  | `Add (position, tile) ->
    add_tile position tile
  | `Update (position, tile) ->
    merge_tile position tile
  | `Move (p1, p2, tile) ->
    move_tile p1 p2 tile

let collector () =
  let document = Html.window ## document in

  let tileContainer = Js.Opt.case
      (document ## querySelector (Js.string ".tile-container"))
      (fun () -> raise Not_found)
      (fun x -> x) in

  let tiles =
    (tileContainer ## querySelectorAll
       (Js.string (".hidden"))) in

  for i = 0 to (tiles ## length) - 1
  do Dom.removeChild tileContainer (nth tiles i); done;

  ()

let schedule lst =
  List.iter
    (fun x ->
       Lwt_js_events.async (fun () ->
           Lwt.return (actuator x)
           >>= fun () -> Lwt_js_events.transitionend (Html.window ## document)))
    lst;
  collector ()

let main _ =
  let (diff, grid) =
    Grid.make 4
    |> Grid.update
    |> (fun (diff, grid) ->
        let (new_diff, new_grid) = Grid.update grid
        in (new_diff :: [ diff ], new_grid))
  in

  let grid = ref grid in
  let score = ref 0 in

  Lwt_js_events.async (fun () -> Lwt.return (schedule diff));

  Lwt_js_events.async (fun () ->
      Lwt_js_events.async_loop
        Lwt_js_events.keydown
        (Html.window ## document)
        (fun event wait ->
           begin
             try let (diff, score', grid') =
               Game.step !score !grid (Game.of_keycode (event ## keyCode)) in
               schedule diff;
               grid := grid';
               score := score';
             with
             | Invalid_argument _ -> ()
             | Game.End_of_game score ->
               (Printf.printf "END OF GAME: %d\n" score)
           end;
           wait));
  Js._false

let () = Html.window ## onload <- Html.handler main
