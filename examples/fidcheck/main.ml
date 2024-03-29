open Oframl

let image_handler (data : string) =
  let str = if String.equal data "initial" then
    "Click to fetch FID"
  else
    [%string "FID: %{data}"]
  in
  [%string
    {|<svg width='191' height='100' xmlns='http://www.w3.org/2000/svg'>
        <rect width='100%' height='100%' fill='black'/>
        <text x='95.5' y='50' text-anchor='middle'
          dominant-baseline='middle'
          font-family='Arial' font-size='12' fill='white'>
          %{str}
        </text>
      </svg>|}]
;;

let frame_handler (): frame = 
  { title = "FID Check"
  ; image_extra_data = "initial"
  ; post_extra_data = ""
  ; buttons = [ { content = "Check FID" } ]
  ; input = None
  }
;;

let post_handler (act : action) (_data : string) : frame =
  { title = "FID Check"
  ; image_extra_data = string_of_int act.fid
  ; post_extra_data = ""
  ; buttons = []
  ; input = None
  }
;;

let base_url = "https://e699-72-69-118-50.ngrok-free.app" in
let start = Server.start base_url 8080 frame_handler post_handler image_handler in
Lwt_main.run start
