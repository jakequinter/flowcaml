open Lwt.Syntax
open Cohttp
open Minttea
open Orgs
open Repos
open Workflows

let get_required_env var =
  match Stdlib.Sys.getenv var with
  | "" -> Fmt.failwith "Empty $%s" var
  | value -> value
  | exception _ -> Fmt.failwith "Missing $%s" var
;;

let highlight fmt = Spices.(default |> fg (color "#FF06B7") |> build) fmt

let highlight_completed fmt =
  Spices.(default |> fg (color "#00FF00") |> build) fmt
;;

let highlight_in_progress fmt =
  Spices.(default |> fg (color "#FFA500") |> build) fmt
;;

(* Load required environment variables *)
let () = Dotenv.export () |> ignore
let gh_api_token = get_required_env "GITHUB_API_TOKEN"
let headers = Header.init_with "Authorization" ("Bearer " ^ gh_api_token)

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
              (let* org_repos = fetch_org_repos headers ~login:org.login in
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
                   headers
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
      let status =
        match workflow.status with
        | "completed" -> highlight_completed "%s" workflow.status
        | "in_progress" -> highlight_in_progress "%s" workflow.status
        | _ -> workflow.status
      in
      Fmt.str "%s %s: %s" cursor status workflow.url
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
  let orgs = Lwt_main.run (fetch_orgs headers) in
  let updated_model = create_initial_model orgs in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model:updated_model
;;
