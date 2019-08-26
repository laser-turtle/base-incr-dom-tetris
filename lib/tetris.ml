open Incr_dom

module Model = struct
  type t = unit
  [@@deriving sexp, compare]

  let init = ()

  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t = unit
  [@@deriving sexp]
end

module State = struct
  type t = unit
end

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let apply_action (model : Model.t) (action : Action.t) _ ~schedule_action =
  model
;;

let view model ~inject =
  let open Incr.Let_syntax in
  let%map model = model in
  Vdom.Node.(div [] [text "Hello World!"])
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

