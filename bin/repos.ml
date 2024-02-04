open Lwt.Syntax
open Cohttp_lwt_unix

type repo = { name : string }

let parse_org_repos json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.to_list
  |> List.map (fun repo ->
    { name =
        repo |> Yojson.Safe.Util.member "name" |> Yojson.Safe.Util.to_string
    })
;;

let fetch_org_repos headers ~login =
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
