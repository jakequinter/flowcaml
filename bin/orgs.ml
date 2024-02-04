open Lwt.Syntax
open Cohttp_lwt_unix

type org = { login : string }

let parse_orgs json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.to_list
  |> List.map (fun org ->
    { login =
        org |> Yojson.Safe.Util.member "login" |> Yojson.Safe.Util.to_string
    })
;;

let fetch_orgs headers =
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
