(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Message = Dream_pure.Message



(* Used for converting the stream interface of [multipart_form] into the pull
   interface of Dream.

   [state] permits to dissociate the initial state made by
   [initial_multipart_state] and one which started to consume the body stream
   (see the call of [Upload.upload]). *)
type multipart_state = {
  mutable state_init : bool;
  parts : upload_part Queue.t;
  mutable current : upload_part option;
  mutable parser : parser_step option;
  mutable finished : bool;
  mutable error : string option;
}

and upload_part = {
  name : string option;
  filename : string option;
  headers : (string * string) list;
  chunks : string Queue.t;
  mutable closed : bool;
}

and parser_step =
  [ `String of string | `Eof ] ->
  [ `Continue | `Done of upload_part Multipart_form.t | `Fail of string ]

let initial_multipart_state () = {
  state_init = true;
  parts = Queue.create ();
  current = None;
  parser = None;
  finished = false;
  error = None;
}

(* TODO Dump the value of the multipart state somehow? *)
let multipart_state_field : multipart_state Message.field =
  Message.new_field
    ~name:"dream.multipart"
    ()

let multipart_state request =
  match Message.field request multipart_state_field with
  | Some state -> state
  | None ->
    let state = initial_multipart_state () in
    Message.set_field request multipart_state_field state;
    state

let field_to_string (request : Message.request) field =
  ignore request;
  let open Multipart_form in
  match field with
  | Field.Field (field_name, Field.Content_type, v) ->
    (field_name :> string), Content_type.to_string v
  | Field.Field (field_name, Field.Content_disposition, v) ->
    (field_name :> string), Content_disposition.to_string v
  | Field.Field (field_name, Field.Content_encoding, v) ->
    (field_name :> string), Content_encoding.to_string v
  | Field.Field (field_name, Field.Field, v) ->
    (field_name :> string), Unstrctrd.to_utf_8_string v

let log = Log.sub_log "dream.upload"

type part = string option * string option * ((string * string) list)

let content_type request =
  match Message.header request "Content-Type" with
  | Some content_type ->
    Result.to_option
      (Multipart_form.Content_type.of_string (content_type ^ "\r\n"))
  | None -> None

let part_of_header request header =
  let disposition = Multipart_form.Header.content_disposition header in
  let name = Option.bind disposition Multipart_form.Content_disposition.name in
  let filename =
    Option.bind disposition Multipart_form.Content_disposition.filename
  in
  {
    name;
    filename;
    headers =
      header
      |> Multipart_form.Header.to_list
      |> List.map (field_to_string request);
    chunks = Queue.create ();
    closed = false;
  }

let fail_wrong_content_type () =
  let message =
    "The request does not have 'Content-Type: multipart/form_data; ...'"
  in
  log.error (fun log -> log "%s" message);
  failwith message

let ensure_parser request state =
  if state.state_init then (
    match content_type request with
    | None -> fail_wrong_content_type ()
    | Some content_type ->
      let emitters header =
        let part = part_of_header request header in
        Queue.add part state.parts;
        ( (function
          | None -> part.closed <- true
          | Some chunk -> Queue.add chunk part.chunks),
          part )
      in
      state.parser <- Some (Multipart_form.parse ~emitters content_type);
      state.state_init <- false)

let feed_parser request state =
  ensure_parser request state;
  match state.error, state.finished, state.parser with
  | Some error, _, _ -> failwith error
  | None, true, _ -> ()
  | None, false, None -> failwith "multipart parser was not initialized"
  | None, false, Some parser -> (
    let input =
      match Message.read (Message.server_stream request) with
      | Some chunk -> `String chunk
      | None -> `Eof
    in
    match parser input with
    | `Continue -> ()
    | `Done _tree -> state.finished <- true
    | `Fail error ->
      state.error <- Some error;
      failwith error)

let part_ready part =
  (not (Queue.is_empty part.chunks)) || part.closed

let rec feed_until_part request state =
  if Queue.is_empty state.parts && not state.finished then (
    feed_parser request state;
    feed_until_part request state)

let rec feed_until_current_ready request state part =
  if not (part_ready part) && not state.finished then (
    feed_parser request state;
    feed_until_current_ready request state part)

let rec state request =
  let multipart = multipart_state request in
  ensure_parser request multipart;
  match multipart.current with
  | Some part when part.closed && Queue.is_empty part.chunks ->
    multipart.current <- None;
    state request
  | Some part ->
    Some (part.name, part.filename, part.headers)
  | None ->
    feed_until_part request multipart;
    if Queue.is_empty multipart.parts then None
    else
      let part = Queue.take multipart.parts in
      multipart.current <- Some part;
      Some (part.name, part.filename, part.headers)

and upload request = state request

let rec upload_part (request : Message.request) =
  let state = multipart_state request in
  match state.current with
  | None -> None
  | Some part when not (Queue.is_empty part.chunks) ->
    Some (Queue.take part.chunks)
  | Some part when part.closed ->
    log.debug (fun m -> m "End of the part.");
    state.current <- None;
    None
  | Some part ->
    feed_until_current_ready request state part;
    upload_part request

type multipart_form =
  (string * ((string option * string) list)) list
module Map = Map.Make (String)

let multipart ?(csrf=true) ~now request =
  match content_type request with
  | None -> `Wrong_content_type
  | Some content_type ->
    let body =
      fun () -> Message.read (Message.server_stream request)
    in
    match Multipart_form.of_stream_to_list body content_type with
    | Error (`Msg _err) ->
      `Wrong_content_type (* XXX(dinosaure): better error? *)
    | Ok (tree, assoc) ->
      let open Multipart_form in
      let tree = flatten tree in
      let fold acc { Multipart_form.header; body= uid; } =
        let contents = List.assoc uid assoc in
        let content_disposition = Header.content_disposition header in
        let filename = Option.bind content_disposition Content_disposition.filename in
        match Option.bind content_disposition Content_disposition.name with
        | None -> acc
        | Some name ->
          let vs =
            match Map.find_opt name acc with
            | Some vs -> vs
            | None -> []
          in
          Map.add name ((filename, contents)::vs) acc
      in
      let parts =
        List.fold_left fold Map.empty tree
        |> Map.bindings
        |> List.map (fun (name, values) ->
          match values with
          | [Some "", ""] -> name, []
          | _ -> name, List.rev values)
      in
      if csrf then
        Form.sort_and_check_form ~now
          (function
          | [None, value] -> value
          | _ -> "")
          parts request
      else
        let form = Form.sort parts in
        `Ok form
