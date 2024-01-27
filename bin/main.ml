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
  { org_cursor : int
  ; org_choices : (org * [ `selected | `unselected ]) list
  ; selected_org : org option
  ; repo_cursor : int
  ; repo_choices : (repo * [ `selected | `unselected ]) list option
  }

let create_initial_model orgs =
  { org_cursor = 0
  ; org_choices = List.map (fun org -> org, `unselected) orgs
  ; selected_org = None
  ; repo_cursor = 0
  ; repo_choices = None
  }
;;

let init _model = Command.Noop

let update event model =
  match event with
  | Event.KeyDown (Key "q" | Escape) -> model, Command.Quit
  | Event.KeyDown (Up | Key "k") ->
    if model.selected_org = None
    then (
      let org_cursor =
        if model.org_cursor = 0
        then List.length model.org_choices - 1
        else model.org_cursor - 1
      in
      { model with org_cursor }, Command.Noop)
    else (
      let repo_cursor =
        if model.repo_cursor = 0
        then List.length (Option.value ~default:[] model.repo_choices) - 1
        else model.repo_cursor - 1
      in
      { model with repo_cursor }, Command.Noop)
  | Event.KeyDown (Down | Key "j") ->
    if model.selected_org = None
    then (
      let org_cursor =
        if model.org_cursor = List.length model.org_choices - 1
        then 0
        else model.org_cursor + 1
      in
      { model with org_cursor }, Command.Noop)
    else (
      let repo_cursor =
        if model.repo_cursor
           = List.length (Option.value ~default:[] model.repo_choices) - 1
        then 0
        else model.repo_cursor + 1
      in
      { model with repo_cursor }, Command.Noop)
  | Event.KeyDown (Enter | Space) ->
    let selected_org_opt =
      match List.nth_opt model.org_choices model.org_cursor with
      | Some (org, _) -> Some org
      | None -> None
    in
    ( (match selected_org_opt with
       | Some org ->
         let updated_model =
           { model with
             selected_org = Some org
           ; repo_choices = None (* Reset repo_choices when selecting an org *)
           }
         in
         Lwt_main.run
           (let* org_repos = fetch_org_repos ~login:org.login in
            Lwt.return
              { updated_model with
                repo_choices =
                  Some (List.map (fun repo -> repo, `unselected) org_repos)
              })
       | None -> model)
    , Command.Noop )
  | _ -> model, Command.Noop
;;

let view model =
  match model.selected_org, model.repo_choices with
  | Some selected_org, Some org_repos ->
    let org_name = selected_org.login in
    let repo_list =
      org_repos
      |> List.mapi (fun idx (repo, _checked) ->
        let cursor = if model.repo_cursor = idx then ">" else " " in
        Printf.sprintf "%s %s" cursor repo.name)
      |> String.concat "\n"
    in
    Printf.sprintf
      {|
Select a repo from %s:

%s

Press q to quit.
|}
      org_name
      repo_list
  | _ ->
    let orgs =
      model.org_choices
      |> List.mapi (fun idx ((org : org), _checked) ->
        let cursor = if model.org_cursor = idx then ">" else " " in
        Printf.sprintf "%s %s" cursor org.login)
      |> String.concat "\n"
    in
    Printf.sprintf {|
Select an organization:

%s

Press q to quit.

|} orgs
;;

let () =
  let orgs = Lwt_main.run fetch_orgs in
  let updated_model = create_initial_model orgs in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model:updated_model
;;
