open Incr_dom

let () =
    Start_app.start
        (module Incr_dom_tetris.Tetris)
        ~bind_to_element_with_id:"app"
        ~initial_model:(Incr_dom_tetris.Tetris.Model.init)



