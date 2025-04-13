open Ppx_yojson_conv_lib.Yojson_conv

(* ---- Fragment serialisation ---- *)

type timeseries = timeseries_beat list list

(* attack, pitch, accent *)
and timeseries_beat = int * int * int [@@deriving yojson]

(* ---- Client to Server messages ---- *)

type client_to_server =
  | Create_lobby_request
  | Join_lobby_request of { pin : int }
  | Lobby_set_ready of { pin : int }
  | Leave_lobby_request of { pin : int }
  | Disband_lobby_request of { pin : int }
  | Start_performance_request of { pin : int; settings : settings }
  | Send_heartbeat of { pin : int; fragment : timeseries }

and settings = { bpm : int; extinction : extinction_settings }

and extinction_settings = { events : int; probability : int; rate : int }
[@@deriving of_yojson]

(* ---- Server to Client messages ---- *)

type lobby_response_data = {
  pin : int;
  ready_players : int;
  total_players : int;
}
[@@deriving yojson_of]

type extinction = { generation : int; extinct : bool } [@@deriving yojson_of]

type server_to_client =
  | Error of string
  | Create_lobby_response of lobby_response_data
  | Lobby_update of lobby_response_data
  | Lobby_disbanded of { pin : int }
  | Start_performance_response of {
      pin : int;
      seed : int;
      start : int;
      bpm : int;
      programme : extinction list;
    }
  | Receive_heartbeat of { fragment : timeseries }
[@@deriving yojson_of]

(* ---- Mutable state ---- *)

module Int_hashtbl = Hashtbl.Make (Int)
module Int_set = Set.Make (Int)

type performance_data = {
  mutable players : Int_set.t;
  start : int;
  bpm : int;
  extinction_probability : float;
  programme : int list;
}

type performance =
  | Lobby of { players : bool Int_hashtbl.t }
  | Performance of performance_data

let clients : Dream.websocket Int_hashtbl.t = Int_hashtbl.create 100
let performances : performance Int_hashtbl.t = Int_hashtbl.create 100

(* ---- WebSocket Server ---- *)

let send (stc : server_to_client) (conn : Dream.websocket) : unit Lwt.t =
  stc |> yojson_of_server_to_client |> Yojson.Safe.to_string |> Dream.send conn

let broadcast (stc : server_to_client) players =
  Int_hashtbl.to_seq players |> Seq.map fst
  |> Seq.filter_map (Int_hashtbl.find_opt clients)
  |> Lwt_seq.of_seq
  |> Lwt_seq.iter_p (send stc)

let mk_lobby_data pin players' : lobby_response_data =
  {
    pin;
    ready_players =
      Int_hashtbl.to_seq_values players' |> Seq.filter Fun.id |> Seq.length;
    total_players = Int_hashtbl.length players';
  }

let remove_player who pin players =
  if Int_hashtbl.mem players who then (
    Int_hashtbl.remove players who;
    if Int_hashtbl.length players = 0 then (
      Int_hashtbl.remove performances pin;
      Lwt.return_unit)
    else
      let data = mk_lobby_data pin players in
      broadcast (Lobby_update data) players)
  else Lwt.return_unit

let update (who : int) (msg : client_to_server) : unit Lwt.t =
  let reply_to = Int_hashtbl.find clients who in

  (* Generate a boolean, true with probability p *)
  let prob (p : float) =
    (* Clamp between 0 and 1 *)
    let () = Random.self_init () in
    let p = max 0.0 (min 1.0 p) in
    Random.float 1.0 < p
  in

  (* Precompute a programme of extinctions *)
  let programme_with (extinction_settings : extinction_settings) : int list =
    if extinction_settings.rate = 0 then []
    else
      (* Convert percentage *)
      let dp = Int.to_float extinction_settings.rate /. 100.0 in
      let epoch () =
        let rec aux p = if prob p then 0 else 1 + aux (p +. dp) in
        aux 0.0
      in
      (* The number of generations in between the extinction events *)
      List.init extinction_settings.events (fun _ -> epoch ())
      (* Prefix sum; the exact generations of extinction events *)
      |> List.fold_left
           (fun (acc, res) x -> (acc + x, res @ [ acc + x ]))
           (0, [])
      |> snd
  in

  (* Generate a client-specific programme, specifying whether the client goes extinct or not *)
  let client_programme_with (p : float) : int list -> extinction list =
    List.map (fun generation -> ({ generation; extinct = prob p } : extinction))
  in

  match msg with
  | Create_lobby_request ->
      let rec unique_pin () =
        let candidate = Random.int 1000000 in
        if Int_hashtbl.mem performances candidate then unique_pin ()
        else candidate
      in
      let pin = unique_pin () in
      let players = Int_hashtbl.create 2 in
      Int_hashtbl.add players who true;
      Int_hashtbl.add performances pin (Lobby { players });
      send
        (Create_lobby_response { pin; ready_players = 1; total_players = 1 })
        reply_to
  | Join_lobby_request { pin } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such lobby") reply_to
      | Some perf -> (
          match perf with
          | Lobby { players } ->
              Int_hashtbl.add players who false;
              let data = mk_lobby_data pin players in
              let%lwt () = send (Lobby_update data) reply_to in
              broadcast (Lobby_update data) players
          | Performance data ->
              send
                (Start_performance_response
                   {
                     pin;
                     start = data.start;
                     bpm = data.bpm;
                     seed = Random.int 1000000000;
                     programme =
                       client_programme_with data.extinction_probability
                         data.programme;
                   })
                reply_to))
  | Lobby_set_ready { pin } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such lobby") reply_to
      | Some perf -> (
          match perf with
          | Lobby { players } ->
              Int_hashtbl.replace players who true;
              let data = mk_lobby_data pin players in
              broadcast (Lobby_update data) players
          | Performance _ -> send (Error "Already started") reply_to))
  | Leave_lobby_request { pin } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such lobby") reply_to
      | Some perf -> (
          match perf with
          | Lobby { players } -> remove_player who pin players
          | Performance perf ->
              perf.players <- Int_set.remove who perf.players;
              Lwt.return_unit))
  | Disband_lobby_request { pin } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such lobby") reply_to
      | Some perf -> (
          match perf with
          | Lobby { players } ->
              Int_hashtbl.remove performances pin;
              broadcast (Lobby_disbanded { pin }) players
          | Performance _ -> send (Error "Already started") reply_to))
  | Start_performance_request { pin; settings } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such lobby") reply_to
      | Some perf -> (
          match perf with
          | Lobby { players } ->
              let now = Int.of_float (Unix.gettimeofday () *. 1000.0) in
              let data : performance_data =
                (* Precompute the extinction event times *)
                {
                  players = Int_hashtbl.to_seq_keys players |> Int_set.of_seq;
                  start = now + 3000;
                  bpm = settings.bpm;
                  extinction_probability =
                    Int.to_float settings.extinction.probability /. 100.0;
                  programme = programme_with settings.extinction;
                }
              in
              Int_hashtbl.replace performances pin (Performance data);
              let mk_resp () : server_to_client =
                let seed = Random.int 1000000000 in
                let programme =
                  client_programme_with data.extinction_probability
                    data.programme
                in
                Start_performance_response
                  { pin; seed; start = data.start; bpm = data.bpm; programme }
              in
              Int_hashtbl.to_seq_keys players
              |> Seq.filter_map (Int_hashtbl.find_opt clients)
              |> Lwt_seq.of_seq
              (* Eta reduction not possible since mk_resp has side effects *)
              |> Lwt_seq.iter_p (fun conn -> send (mk_resp ()) conn)
          | Performance _ -> send (Error "Performance already started") reply_to
          ))
  | Send_heartbeat { pin; fragment } -> (
      match Int_hashtbl.find_opt performances pin with
      | None -> send (Error "No such performance") reply_to
      | Some perf -> (
          match perf with
          | Lobby _ -> send (Error "Not started yet") reply_to
          | Performance perf ->
              perf.players |> Int_set.remove who |> Int_set.to_list
              |> List.filter_map (Int_hashtbl.find_opt clients)
              |> Lwt_list.iter_p (send (Receive_heartbeat { fragment }))))

let ws_server : Dream.websocket -> unit Lwt.t =
  let id = ref 0 in
  fun (conn : Dream.websocket) ->
    let who = !id in
    id := !id + 1;
    Int_hashtbl.add clients who conn;

    let rec loop () =
      match%lwt Dream.receive conn with
      | None -> Lwt.return_unit
      | Some message -> (
          Dream.log "%s\n\n" message;
          try
            let json = Yojson.Safe.from_string message in
            let parsed_json = client_to_server_of_yojson json in
            match parsed_json with
            | cts ->
                let%lwt () =
                  try update who cts
                  with _ -> send (Error "Internal error") conn
                in
                loop ()
          with _ -> send (Error "Internal error: fatal") conn)
    in

    let%lwt () = loop () in
    Int_hashtbl.remove clients who;
    let%lwt () =
      Int_hashtbl.to_seq performances
      |> Seq.map (function
           | pin, Lobby { players } -> remove_player who pin players
           | _, Performance data ->
               data.players <- Int_set.remove who data.players;
               Lwt.return_unit)
      |> Lwt_seq.of_seq |> Lwt_seq.iter_p Fun.id
    in
    Dream.close_websocket conn

let () =
  Random.self_init ();
  Dream.router
    [
      Dream.get "/ws" (fun _ -> Dream.websocket ws_server);
      Dream.get "/hello" (fun _ -> Dream.html "Hello, world!");
    ]
  |> Dream.logger
  |> Dream.run ~port:3000 ~interface:"localhost"
