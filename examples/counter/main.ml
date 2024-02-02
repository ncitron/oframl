open Oframl

let counter = ref 0

let image_handler (_data : string) =
  {|
      <svg width='191' height='100' xmlns='http://www.w3.org/2000/svg'>
        <rect width='100%' height='100%' fill='black'/>
        <text x='95.5' y='40' text-anchor='middle' 
          dominant-baseline='middle' 
          font-family='Arial' font-size='12' fill='white'>
          This Frame is Written in OCaml
        </text>
        <text x='95.5' y='60' text-anchor='middle' 
          dominant-baseline='middle' 
          font-family='Arial' font-size='12' fill='white'>
          counter: |}
  ^ string_of_int !counter
  ^ {|</text>
      </svg>
    |}
;;

let frame () : frame =
  { title = "Simple Frame"
  ; image_extra_data = ""
  ; post_extra_data = ""
  ; buttons = [ { content = "+" }; { content = "-" } ]
  }
;;

let frame_handler () = frame ()

let post_handler (act : action) (_data : string) : frame =
  let () =
    if act.button_index = 1 then counter := !counter + 1 else counter := !counter - 1
  in
  frame ()
;;

let base_url = " https://df78-72-69-118-50.ngrok-free.app" in
let start = Server.start base_url frame_handler post_handler image_handler in
Lwt_main.run start

