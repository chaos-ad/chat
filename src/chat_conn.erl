-module(chat_conn).

-export([init/2, handle_request/2, terminate/2, decode/2, encode/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(UserID, _Options) ->
    case chat_user_sup:start_user(UserID, self()) of
        {ok, UserPid} -> {ok, UserPid};
        {error, {already_started, UserPid}} ->
            ok = chat_user:attach(UserPid),
            {ok, UserPid};
        Other ->
            Other
    end.

handle_request(Request, UserPid) ->
    case chat_user:request(UserPid, Request) of
        ok -> {ok, UserPid};
        {ok, Response} -> {ok, Response, UserPid};
        {error, Error} -> {error, Error}
    end.

terminate(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Encoding, {request, Request}) ->
    chat_piqi:parse_client2chat(Request, Encoding).
encode(Encoding, {response, Response}) ->
    iolist_to_binary(chat_piqi:gen_chat2client(Response, Encoding));
encode(Encoding, {event, Event}) ->
    iolist_to_binary(chat_piqi:gen_event(Event, Encoding)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
