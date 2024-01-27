[@@@warning "-32"]

open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
open Minttea

let get_required_env var =
  match Stdlib.Sys.getenv var with
  | "" -> Fmt.failwith "Empty $%s" var
  | value -> value
  | exception _ -> Fmt.failwith "Missing $%s" var
;;

(* Load required environment variables *)
let () = Dotenv.export () |> ignore
let gh_api_token = get_required_env "GITHUB_API_TOKEN"

type org = { login : string }
type repo = { name : string }

let parse_orgs json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.to_list
  |> List.map (fun org ->
    { login =
        org |> Yojson.Safe.Util.member "login" |> Yojson.Safe.Util.to_string
    })
;;

let headers = Header.init_with "Authorization" ("Bearer " ^ gh_api_token)

let fetch_orgs =
  let* _, body =
    Client.get ~headers (Uri.of_string "https://api.github.com/user/orgs")
  in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (parse_orgs body)
;;

let parse_org_repos json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.to_list
  |> List.map (fun repo ->
    { name =
        repo |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string
    })
;;

let fetch_org_repos ~login =
  let* resp, body =
    Client.get
      ~headers
      (Uri.of_string ("https://api.github.com/orgs/" ^ login ^ "/repos"))
  in
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (parse_org_repos body)
;;

type model =
  { choices : (org * [ `selected | `unselected ]) list
  ; cursor : int
  ; selected_org : org option (* Add selected_org field *)
  ; org_repos : repo list option (* Add org_repos field *)
  }

let create_initial_model orgs =
  { cursor = 0
  ; choices = List.map (fun org -> org, `unselected) orgs
  ; selected_org = None
  ; org_repos = None
  }
;;

let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown (Key "q" | Escape) -> model, Command.Quit
  | Event.KeyDown (Up | Key "k") ->
    let cursor =
      if model.cursor = 0
      then List.length model.choices - 1
      else model.cursor - 1
    in
    { model with cursor }, Command.Noop
  | Event.KeyDown (Down | Key "j") ->
    let cursor =
      if model.cursor = List.length model.choices - 1
      then 0
      else model.cursor + 1
    in
    { model with cursor }, Command.Noop
  | Event.KeyDown (Enter | Space) ->
    let selected_org_opt =
      match List.nth_opt model.choices model.cursor with
      | Some (org, _) -> Some org
      | None -> None
    in
    ( (match selected_org_opt with
       | Some org ->
         Lwt_main.run
           (let* org_repos = fetch_org_repos ~login:org.login in
            Lwt.return
              { model with selected_org = Some org; org_repos = Some org_repos })
       | None -> model)
    , Command.Noop )
  | _ -> model, Command.Noop
;;

let view model =
  match model.selected_org, model.org_repos with
  | Some selected_org, Some org_repos ->
    let org_name = selected_org.login in
    let repo_list =
      org_repos |> List.map (fun repo -> repo.name) |> String.concat "\n"
    in
    Printf.sprintf
      {|
Selected organization: %s

Repositories:
%s

Press q to quit.
|}
      org_name
      repo_list
  | _ ->
    let options =
      model.choices
      |> List.mapi (fun idx ((org : org), checked) ->
        let cursor = if model.cursor = idx then ">" else " " in
        let checked = if checked = `selected then "x" else " " in
        Printf.sprintf "%s [%s] %s" cursor checked org.login)
      |> String.concat "\n"
    in
    Printf.sprintf {|
Select an organization:

%s

Press q to quit.

|} options
;;

let () =
  let orgs = Lwt_main.run fetch_orgs in
  let updated_model = create_initial_model orgs in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model:updated_model
;;
