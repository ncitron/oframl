open Oframl

type message =
  { text : string
  ; x : int
  ; y : int
  ; fid : int
  }

let render_message msg =
  let x = string_of_int msg.x in
  let y = string_of_int msg.y in
  [%string
    {|
    <text x='%{x}' y='%{y}' font-family='Arial' font-size='40' fill='white'>
      %{msg.text}
    </text>
  |}]
;;

let messages = ref []

let image_handler (_data : string) =
  let msgs = List.fold_left (fun acc msg -> acc ^ render_message msg) "" !messages in
  [%string
    {|<svg width='1910' height='1000' xmlns='http://www.w3.org/2000/svg'>
        <rect width='100%' height='100%' fill='black'/>
        %{msgs}
      </svg>|}]
;;

let frame_handler () : frame =
  { title = "Graffiti"
  ; image_extra_data = ""
  ; post_extra_data = ""
  ; buttons = [ { content = "Add Graffiti" } ]
  ; input = Some "message"
  }
;;

let post_handler (act : action) (_data : string) : frame =
  let fids = List.map (fun msg -> msg.fid) !messages in
  let () = if Option.is_none (List.find_opt (fun fid -> act.fid = fid) fids) then
    let x = Random.int 1910 in
    let y = Random.int 1000 in
    let txt = match act.input_text with
    | Some txt -> txt
    | None -> ""
    in
    let msg = { x = x; y = y; text = txt; fid = act.fid} in
    messages := msg :: !messages
  else
    ()
  in

  { title = "Echo"
  ; image_extra_data = Option.get act.input_text
  ; post_extra_data = ""
  ; buttons = []
  ; input = None
  }
;;

let base_url = "https://e699-72-69-118-50.ngrok-free.app" in
let start = Server.start base_url 8080 frame_handler post_handler image_handler in
Lwt_main.run start
