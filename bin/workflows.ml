open Lwt.Syntax
open Cohttp_lwt_unix

type workflow =
  { status : string
  ; url : string
  }

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

let fetch_workflows headers ~login ~repo =
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
