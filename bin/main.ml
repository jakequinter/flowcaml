open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

let get_required_env var =
  match Stdlib.Sys.getenv var with
  | "" -> Fmt.failwith "Empty $%s" var
  | value -> value
  | exception _ -> Fmt.failwith "Missing $%s" var
;;

(* Load required environment variables *)
let () = Dotenv.export () |> ignore
let gh_api_token = get_required_env "GITHUB_API_TOKEN"

type org =
  { login : string
  ; id : int
  }

let parse_orgs json_str =
  let json = Yojson.Safe.from_string json_str in
  json
  |> Yojson.Safe.Util.to_list
  |> List.map (fun org ->
    { login =
        org |> Yojson.Safe.Util.member "login" |> Yojson.Safe.Util.to_string
    ; id = org |> Yojson.Safe.Util.member "id" |> Yojson.Safe.Util.to_int
    })
;;

let fetch_orgs =
  let headers = Header.init_with "Authorization" ("Bearer " ^ gh_api_token) in
  let* resp, body =
    Client.get ~headers (Uri.of_string "https://api.github.com/user/orgs")
  in
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (parse_orgs body)
;;

let () =
  let orgs = Lwt_main.run fetch_orgs in
  List.iter (fun org -> Format.printf "Org: %s, ID: %d\n" org.login org.id) orgs
;;
