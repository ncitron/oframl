(* Button module *)
module Button = struct
  type t = { content : string }

  let render button index =
    Printf.sprintf
      "<meta property='fc:frame:button:%i' content='%s'/>"
      index
      button.content
  ;;
end

type button = Button.t

(* Frame module *)
module Frame = struct
  type t =
    { title : string
    ; image_extra_data : string
    ; post_extra_data : string
    ; buttons : button list
    }

  let render frame base_url =
    let buttons = List.mapi (fun i btn -> i + 1, btn) frame.buttons in
    let buttons =
      List.fold_left (fun acc (i, btn) -> acc ^ Button.render btn i) "" buttons
    in
    let image_url =
      base_url ^ "/image/" ^ string_of_float (Unix.time ()) ^ "/" ^ frame.image_extra_data
    in
    let post_url = base_url ^ "/post/" ^ frame.post_extra_data in
    Printf.sprintf
      {|
        <!DOCTYPE html>
        <html>
          <head>
            <meta property='og:title' content='%s'/>
            <meta property='og:image' content='%s' />
  	  		  <meta property='fc:frame' content='vNext' />
  	  		  <meta property='fc:frame:image' content='%s'/>
            <meta property='fc:frame:post_url' content='%s' />
            %s
          </head>
        </html>
      |}
      frame.title
      image_url
      image_url
      post_url
      buttons
  ;;
end

type frame = Frame.t

(* Action module *)
module Action = struct
  type t = { button_index : int [@key "buttonIndex"] }
  [@@deriving yojson { strict = false }]
end

type action = Action.t

type post = { untrusted_data : Action.t [@key "untrustedData"] }
[@@deriving yojson { strict = false }]

(* Server module *)
module Server = struct
  let start
    (base_url : string)
    (frame_handler : unit -> frame)
    (post_handler : action -> string -> frame)
    (image_handler : string -> string)
    =
    let handler _con req body =
      let open Cohttp in
      let uri = req |> Request.uri |> Uri.path in
      let uri = List.tl (String.split_on_char '/' uri) in
      match uri with
      | [ "frame" ] ->
        Cohttp_lwt_unix.Server.respond_string
          ~status:`OK
          ~body:(Frame.render (frame_handler ()) base_url)
          ()
      | [ "image"; _; data] ->
        let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
        let headers = Cohttp.Header.add headers "Cache-Control" "no-cache" in
        Cohttp_lwt_unix.Server.respond_string
          ~status:`OK
          ~body:(image_handler data)
          ~headers
          ()
      | [ "post"; data ] ->
        let open Lwt.Syntax in
        let* body = Cohttp_lwt.Body.to_string body in
        let json = Yojson.Safe.from_string body in
        let action =
          match post_of_yojson json with
          | Ok post -> post.untrusted_data
          | Error e -> raise (Failure e)
        in
        Cohttp_lwt_unix.Server.respond_string
          ~status:`OK
          ~body:(Frame.render (post_handler action data) base_url)
          ()
      | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"not found" ()
    in
    let open Lwt.Syntax in
    let server = Cohttp_lwt_unix.Server.make ~callback:handler () in
    let* ctx = Conduit_lwt_unix.init ~src:"0.0.0.0" () in
    let ctx = Cohttp_lwt_unix.Client.custom_ctx ~ctx () in
    Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port 8000)) server
  ;;
end
