-module(chat_user).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/2]).
-export([attach/1, detach/1, request/2]).

-include("piqi/chat_piqi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SHUTDOWN_TIMEOUT, 5000).

-record(state, {userid, connections=gb_sets:new()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(UserID, ConnPid) ->
    gen_server:start_link({global, {?MODULE, UserID}}, ?MODULE, [UserID, ConnPid], []).

attach(UserPid) ->
    gen_server:call(UserPid, attach).

detach(UserPid) ->
    gen_server:call(UserPid, detach).

request(UserPid, Request) ->
    gen_server:call(UserPid, Request).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([UserID, ConnPid]) ->
    erlang:link(ConnPid),
    lager:debug("User '~s': started", [UserID]),
    {ok, #state{userid=UserID, connections=gb_sets:from_list([ConnPid])}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(attach, {ConnPid, _}, State=#state{userid=UserID}) ->
    try
        NewConnections = gb_sets:insert(ConnPid, State#state.connections),
        erlang:link(ConnPid),
        lager:debug("['~s']: process ~p attached", [UserID, ConnPid]),
        {reply, ok, State#state{connections=NewConnections}}
    catch
        error:{key_exists, ConnPid} ->
            {reply, {error, already_attached}, State}
    end;

handle_call(detach, {ConnPid, _}, State=#state{userid=UserID, connections=Connections}) ->
    try
        NewConnections = gb_sets:delete(ConnPid, Connections),
        erlang:unlink(ConnPid),
        lager:debug("['~s']: process ~p detached", [UserID, ConnPid]),
        case gb_sets:is_empty(NewConnections) of
            true -> {stop, normal, ok, State#state{connections=NewConnections}};
            false -> {reply, ok, State#state{connections=NewConnections}}
        end
    catch
        error:function_clause ->
            {reply, {error, not_attached}, State}
    end;

handle_call({say, #say{to={user, To}, body=Body}}, {ConnPid, _}, State=#state{userid=UserID}) ->
    case gb_sets:is_element(ConnPid, State#state.connections) of
        false -> {reply, {error, not_attached}, State};
        true ->
            case global:whereis_name({chat_user, To}) of
                undefined -> {reply, {error, invalid_request}, State};
                UserPid -> 
                    UserPid ! {event, {message, #message{from=UserID, to={user, To}, body=Body}}},
                    {reply, ok, State}
            end
    end;

handle_call({say, #say{to={group, To}, body=Body}}, {ConnPid, _}, State=#state{userid=UserID}) ->
    case gb_sets:is_element(ConnPid, State#state.connections) of
        false -> {reply, {error, not_attached}, State};
        true -> {reply, chat_group:send_message(To, UserID, Body), State}
    end;

handle_call({join_group, GroupID}, {ConnPid, _}, State=#state{userid=UserID}) ->
    case gb_sets:is_element(ConnPid, State#state.connections) of
        false -> {reply, {error, not_attached}, State};
        true -> {reply, chat_group:add_user(GroupID, UserID, self()), State}
    end;

handle_call({leave_group, GroupID}, {ConnPid, _}, State=#state{userid=UserID}) ->
    case gb_sets:is_element(ConnPid, State#state.connections) of
        false -> {reply, {error, not_attached}, State};
        true -> {reply, chat_group:del_user(GroupID, UserID, self()), State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(_Cast, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({event, Event}, State=#state{connections=Connections}) ->
    [ Pid ! {event, Event} || Pid <- gb_sets:to_list(Connections) ],
    {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
    case handle_call(detach, {Pid, undefined}, State) of
        {reply, _, NewState} -> {noreply, NewState};
        {stop, Reason, _, NewState} -> {stop, Reason, NewState}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(Reason, _) ->
    lager:debug("~p: ~p terminated with reason ~p", [?MODULE, self(), Reason]).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
