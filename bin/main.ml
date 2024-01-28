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
  let* _, body =
    Client.get
      ~headers
      (Uri.of_string ("https://api.github.com/orgs/" ^ login ^ "/repos"))
  in
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
  let* _, body =
    Client.get
      ~headers
      (Uri.of_string
         ("https://api.github.com/repos/"
          ^ login
          ^ "/"
          ^ repo
          ^ "/actions/runs?per_page=5"))
  in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (parse_workflows body)
;;

type context =
  | Orgs
  | Repos
  | Workflows

type choice =
  | Org of org
  | Repo of repo
  | Workflow of workflow

type model =
  { cursor : int
  ; context : context
  ; choices : (choice * [ `selected | `unselected ]) list
  ; selected_org : org option
  ; selected_repo : repo option
  }

let create_initial_model orgs =
  { cursor = 0
  ; context = Orgs
  ; choices = List.map (fun org -> Org org, `unselected) orgs
  ; selected_org = None
  ; selected_repo = None
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
    ( (match model.context with
       | Orgs ->
         (match List.nth_opt model.choices model.cursor with
          | Some (Org org, _) ->
            let updated_model =
              { model with selected_org = Some org; context = Repos }
            in
            Lwt_main.run
              (let* org_repos = fetch_org_repos ~login:org.login in
               Lwt.return
                 { updated_model with
                   cursor = 0
                 ; choices =
                     List.map (fun repo -> Repo repo, `unselected) org_repos
                 })
          | _ -> model)
       | Repos ->
         (match List.nth_opt model.choices model.cursor with
          | Some (Repo repo, _) ->
            let updated_model =
              { model with selected_repo = Some repo; context = Workflows }
            in
            Lwt_main.run
              (let* workflows =
                 fetch_workflows
                   ~login:(Option.get model.selected_org).login
                   ~repo:repo.name
               in
               Lwt.return
                 { updated_model with
                   choices =
                     List.map
                       (fun workflow -> Workflow workflow, `unselected)
                       workflows
                 })
          | _ -> model)
       | Workflows -> model)
    , Command.Noop )
  | _ -> model, Command.Noop
;;

let view model =
  let render_choice idx (choice, _) =
    match choice with
    | Org org ->
      let cursor = if idx = model.cursor then highlight ">" else " " in
      Fmt.str "%s %s" cursor org.login
    | Repo repo ->
      let cursor = if idx = model.cursor then highlight ">" else " " in
      Fmt.str "%s %s" cursor repo.name
    | Workflow workflow ->
      let cursor = if idx = model.cursor then highlight ">" else " " in
      Fmt.str "%s status: %s, url: %s" cursor workflow.status workflow.url
  in
  let choices_str =
    model.choices |> List.mapi render_choice |> String.concat "\n"
  in
  match model.context with
  | Orgs ->
    Fmt.str {|
Select an organization:

%s

Press q to quit.
|} choices_str
  | Repos ->
    let org_name =
      match model.selected_org with
      | Some org -> org.login
      | None -> ""
    in
    Fmt.str {|
%s repos:

%s

Press q to quit.
|} org_name choices_str
  | Workflows ->
    let repo_name =
      match model.selected_repo with
      | Some repo -> repo.name
      | None -> ""
    in
    Fmt.str {|
%s workflows:

%s

Press q to quit.
|} repo_name choices_str
;;

let () =
  let orgs = Lwt_main.run fetch_orgs in
  let updated_model = create_initial_model orgs in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model:updated_model
;;
