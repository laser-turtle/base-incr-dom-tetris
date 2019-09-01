open Incr_dom

module Model = struct
  type page =
    | MainMenu
    | NewGame
    | HighScores
    | Credits
    [@@deriving sexp, compare]

  type t = {
    current_page : page;
  }
  [@@deriving sexp, compare]

  let init = {
    current_page = MainMenu
  }

  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t = ChangePage of Model.page
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let apply_action (model : Model.t) (action : Action.t) _ ~schedule_action =
  match action with
  | ChangePage new_page ->
    Model.{ current_page = new_page }
;;

let subpage page_name content inject =
  let open Vdom in
  let open Node in
  let open Attr in
  let outer_padding = 
    let pad = `Rem 0.5 in
    Css_gen.(padding 
               ~top:pad
               ~bottom:pad
               ~left:pad
               ~right:pad
               ()
            )
  in
  div [class_ "container"] [
    div [class_ "content";
         style outer_padding]
        [
          div 
            [style Css_gen.(display `Inline_block)] 
            [h2 [] [text page_name]];

          button 
            [classes ["button"; "is-info"];
             style Css_gen.(float `Right);
             on_click (fun _ -> inject Action.(ChangePage MainMenu))] 
            [text "Back"];

          content;
        ]
  ]
;;

let main_menu inject =
  let open Vdom in
  let open Node in
  let open Attr in
  let field content =
    div [class_ "field"] [
      div [class_ "control"] [
        content
      ]
    ]
  in

  let menu_button content (page : Model.page) =
    button [
      classes ["button"; "is-info"; "is-centered"];
      style Css_gen.(
          display `Block
          @> margin ~bottom:(`Rem 0.5) ()
          @> width (`Percent Percent.(of_percentage 100.))
        );
      on_click (fun _ -> inject Action.(ChangePage page));
    ] [text content]
  in

  div [classes ["container"; "has-text-centered"]] [
    div [class_ "content";
         style Css_gen.(
             width (`Percent Percent.(of_percentage 25.))
             @> display `Inline_block
             @> padding ~top:(`Rem 2.) ()
           )
        ] [
      div [classes ["has-text-centered"]] [
        h1 [] [text "Tetris"];
      ];
      field (menu_button "New Game" Model.NewGame);
      field (menu_button "High Scores" Model.HighScores);
      field (menu_button "Credits" Model.Credits);
    ]
  ]
;;

let new_game_page = 
  subpage "New Game" Vdom.Node.(div [] [])
;;

let high_scores_page = 
  subpage "High Scores" Vdom.Node.(div [] [])
;;

let credits_page =
  subpage "Credits" Vdom.Node.(div [] [])
;;

let view model ~inject =
  let open Incr.Let_syntax in
  let%map current_page =
    model >>| (fun m -> m.Model.current_page) 
  in
  match current_page with
  | MainMenu -> main_menu inject
  | NewGame -> new_game_page inject
  | HighScores -> high_scores_page inject
  | Credits -> credits_page inject
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
;;

