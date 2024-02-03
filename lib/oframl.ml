(* Button module *)
module Button = struct
  type t = { content : string }

  let render button index =
    [%string
      "<meta property='fc:frame:button:%{index#Int}' content='%{button.content}'/>"]
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
    ; input : string option
    }

  let render frame base_url =
    let buttons = List.mapi (fun i btn -> i + 1, btn) frame.buttons in
    let buttons =
      List.fold_left (fun acc (i, btn) -> acc ^ Button.render btn i) "" buttons
    in
    let image_url =
      base_url ^ "/image/" ^ string_of_float (Unix.time ()) ^ "/" ^ frame.image_extra_data
    in
    let input_tag =
      match frame.input with
      | Some input ->
        [%string "<meta property='fc:frame:input:text' content='%{input}' />"]
      | None -> ""
    in
    let post_url = base_url ^ "/post/" ^ frame.post_extra_data in
    [%string
      {|<!DOCTYPE html>
        <html>
          <head>
            <meta property='og:title' content='%{frame.title}'/>
            <meta property='og:image' content='%{image_url}' />
            <meta property='fc:frame' content='vNext' />
            <meta property='fc:frame:image' content='%{image_url}'/>
            <meta property='fc:frame:post_url' content='%{post_url}' />
            %{input_tag}
            %{buttons}
            </head>
        </html>|}]
  ;;
end

type frame = Frame.t

(* Action module *)
module Action = struct
  type t =
    { button_index : int [@key "buttonIndex"]
    ; input_text : string option [@default None] [@key "inputText"]
    ; fid : int
    }
  [@@deriving yojson { strict = false }]
end

type action = Action.t

type post = { untrusted_data : Action.t [@key "untrustedData"] }
[@@deriving yojson { strict = false }]

(* Server module *)
module Server = struct
  let start
    (base_url : string)
    (port : int)
    (frame_handler : unit -> frame)
    (post_handler : action -> string -> frame)
    (image_handler : string -> string)
    =
    let handler _con req body =
      let open Cohttp in
      let path = req |> Request.uri |> Uri.path in
      let path = List.tl (String.split_on_char '/' path) in
      let base_path = base_url |> Uri.of_string |> Uri.path in
      let base_path = List.tl (String.split_on_char '/' base_path) in
      let path = List.filteri (fun i p -> (List.nth_opt base_path i) <> (Some p)) path in
      match path with
      | [ "frame" ] ->
        Cohttp_lwt_unix.Server.respond_string
          ~status:`OK
          ~body:(Frame.render (frame_handler ()) base_url)
          ()
      | [ "image"; _; data ] ->
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
    Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port)) server
  ;;
end

(* Utils module *)
module Utils = struct
  let sanitize_text txt =
    let buf = Buffer.create (String.length txt) in
    let () =
      String.iter
        (fun c ->
          match c with
          | '&' -> Buffer.add_string buf "&amp;"
          | '<' -> Buffer.add_string buf "&lt;"
          | '>' -> Buffer.add_string buf "&gt;"
          | '"' -> Buffer.add_string buf "&quot;"
          | '\'' -> Buffer.add_string buf "&apos;"
          | c -> Buffer.add_char buf c)
        txt
    in
    Buffer.contents buf
  ;;
end

