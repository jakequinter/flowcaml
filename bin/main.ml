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

let highlight fmt = Spices.(default |> fg (color "#FF06B7") |> build) fmt

(* Load required environment variables *)
let () = Dotenv.export () |> ignore
let gh_api_token = get_required_env "GITHUB_API_TOKEN"

type org = { login : string }
type repo = { name : string }

type workflow =
  { status : string
  ; url : string
  }

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
  (* Lwt.return (parse_orgs body) *)
  parse_orgs body
  |> List.sort (fun a b ->
    String.compare
      (String.lowercase_ascii a.login)
      (String.lowercase_ascii b.login))
  |> Lwt.return
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
  parse_org_repos body
  |> List.sort (fun a b ->
    String.compare
      (String.lowercase_ascii a.name)
      (String.lowercase_ascii b.name))
  |> Lwt.return
;;

let parse_workflows json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.member "workflow_runs"
  |> Yojson.Safe.Util.to_list
  |> List.map (fun workflow ->
    { status =
        workflow
        |> Yojson.Safe.Util.member "status"
        |> Yojson.Safe.Util.to_string
    ; url =
        workflow
        |> Yojson.Safe.Util.member "html_url"
        |> Yojson.Safe.Util.to_string
    })
;;

let fetch_workflows ~login ~repo =
  let* resp, body =
    Client.get
      ~headers
      (Uri.of_string
         ("https://api.github.com/repos/"
          ^ login
          ^ "/"
          ^ repo
          ^ "/actions/runs?per_page=5"))
  in
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (parse_workflows body)
;;

type choice =
  | OrgChoice of org * [ `selected | `unselected ]
  | RepoChoice of repo * [ `selected | `unselected ]
  | WorkflowChoice of workflow * [ `selected | `unselected ]

type model =
  { org_cursor : int
  ; choices : choice list
  ; selected_org : org option
  ; repo_cursor : int
  ; selected_repo : repo option
  ; selected_workflow : workflow option
  }

let create_initial_model orgs =
  { org_cursor = 0
  ; choices = List.map (fun org -> OrgChoice (org, `unselected)) orgs
  ; selected_org = None
  ; repo_cursor = 0
  ; selected_repo = None
  ; selected_workflow = None
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
        then List.length model.choices - 1
        else model.org_cursor - 1
      in
      { model with org_cursor }, Command.Noop)
    else (
      let repo_cursor =
        if model.repo_cursor = 0
        then List.length model.choices - 1
        else model.repo_cursor - 1
      in
      { model with repo_cursor }, Command.Noop)
  | Event.KeyDown (Down | Key "j") ->
    if model.selected_org = None
    then (
      let org_cursor =
        if model.org_cursor = List.length model.choices - 1
        then 0
        else model.org_cursor + 1
      in
      { model with org_cursor }, Command.Noop)
    else (
      let repo_cursor =
        if model.repo_cursor = List.length model.choices - 1
        then 0
        else model.repo_cursor + 1
      in
      { model with repo_cursor }, Command.Noop)
  | Event.KeyDown (Enter | Space) ->
    let selected_org_opt =
      match List.nth_opt model.choices model.org_cursor with
      | Some (OrgChoice (org, _)) -> Some org
      | _ -> None
    in
    let selected_repo_opt =
      match List.nth_opt model.choices model.repo_cursor with
      | Some (RepoChoice (repo, _)) -> Some repo
      | _ -> None
    in
    ( (match selected_org_opt, selected_repo_opt with
       | Some org, None ->
         let updated_model = { model with selected_org = Some org } in
         Lwt_main.run
           (let* org_repos = fetch_org_repos ~login:org.login in
            Lwt.return
              { updated_model with
                choices =
                  List.map
                    (fun repo -> RepoChoice (repo, `unselected))
                    org_repos
              })
       | Some org, Some repo ->
         let updated_model =
           { model with selected_org = Some org; selected_repo = Some repo }
         in
         Lwt_main.run
           (let* workflows = fetch_workflows ~login:org.login ~repo:repo.name in
            Lwt.return
              { updated_model with
                choices =
                  List.map
                    (fun workflow -> WorkflowChoice (workflow, `unselected))
                    workflows
              })
       | _ -> model)
    , Command.Noop )
  | _ -> model, Command.Noop
;;

let view model =
  match model.selected_org, model.selected_repo, model.selected_workflow with
  | Some selected_org, None, None ->
    let org_name = selected_org.login in
    let repo_list =
      model.choices
      |> List.mapi (fun idx choice ->
        match choice with
        | RepoChoice (repo, _checked) ->
          let cursor = if model.repo_cursor = idx then highlight ">" else " " in
          Printf.sprintf "%s %s" cursor repo.name
        | _ -> "")
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
  | Some _, Some selected_repo, None ->
    let repo_name = selected_repo.name in
    let workflow_list =
      model.choices
      |> List.mapi (fun idx choice ->
        match choice with
        | WorkflowChoice (workflow, _checked) ->
          let cursor = if model.repo_cursor = idx then highlight ">" else " " in
          Printf.sprintf "%s %s %s" cursor workflow.status workflow.url
        | _ -> "")
      |> String.concat "\n"
    in
    Printf.sprintf
      {|
%s workflows:

%s

Press q to quit.
|}
      repo_name
      workflow_list
  | _ ->
    let orgs =
      model.choices
      |> List.mapi (fun idx choice ->
        match choice with
        | OrgChoice (org, _checked) ->
          let cursor = if model.org_cursor = idx then highlight ">" else " " in
          Printf.sprintf "%s %s" cursor org.login
        | _ -> "")
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
