-module(chat_group).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% gen_server callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api:
-export([start_link/3]).
-compile(export_all).

-include("piqi/chat_piqi.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {groupid, owner, uid2pid=dict:new(), pid2uid=dict:new()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(GroupID, OwnerPid, Options) ->
    gen_server:start_link({global, {?MODULE, GroupID}}, ?MODULE, [GroupID, OwnerPid, Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_user(GroupID, UserID, UserPid) ->
    try gen_server:call({global, {?MODULE, GroupID}}, {add_user, UserID, UserPid})
    catch
        exit:{noproc, _} ->
            {error, {invalid_request, <<"group not found">>}}
    end.

del_user(GroupID, UserID, UserPid) ->
    try gen_server:call({global, {?MODULE, GroupID}}, {del_user, UserID, UserPid})
    catch
        exit:{noproc, _} ->
            {error, {invalid_request, <<"group not found">>}}
    end.

get_users(GroupID) ->
    try gen_server:call({global, {?MODULE, GroupID}}, get_users)
    catch
        exit:{noproc, _} ->
            {error, {invalid_request, <<"group not found">>}}
    end.

send_message(GroupID, From, Message) ->
    try gen_server:call({global, {?MODULE, GroupID}}, {send_message, From, Message})
    catch
        exit:{noproc, _} ->
            {error, {invalid_request, <<"group not found">>}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([GroupID, OwnerPid, _Options]) ->
    erlang:process_flag(trap_exit, true),
    case is_pid(OwnerPid) of
        true -> erlang:link(OwnerPid);
        false -> ok
    end,
    {ok, #state{groupid=GroupID, owner=OwnerPid}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({add_user, UserID, UserPid}, _, State=#state{groupid=GroupID}) ->
    case dict:find(UserID, State#state.uid2pid) of
        {ok, UserPid} ->
            {reply, ok, State};
        error ->
            erlang:link(UserPid),
            NewUid2Pid = dict:store(UserID, UserPid, State#state.uid2pid),
            NewPid2Uid = dict:store(UserPid, UserID, State#state.pid2uid),
            NewState = State#state{uid2pid=NewUid2Pid, pid2uid=NewPid2Uid},
            Event = {user_joined_group, #user_joined_group{groupid=GroupID, userid=UserID}},
            broadcast(Event, State),
            {reply, ok, NewState}
    end;

handle_call({del_user, UserID, UserPid}, _, State=#state{groupid=GroupID}) ->
    case dict:find(UserID, State#state.uid2pid) of
        error ->
            {reply, ok, State};
        {ok, UserPid} ->
            erlang:unlink(UserPid),
            NewUid2Pid = dict:erase(UserID, State#state.uid2pid),
            NewPid2Uid = dict:erase(UserPid, State#state.pid2uid),
            NewState = State#state{uid2pid=NewUid2Pid, pid2uid=NewPid2Uid},
            Event = {user_left_group, #user_left_group{groupid=GroupID, userid=UserID}},
            broadcast(Event, NewState),
            {reply, ok, NewState}
    end;

handle_call(get_users, _, State) ->
    {reply, lists:sort(dict:fetch_keys(State#state.uid2pid)), State};

handle_call({send_message, UserID, Message}, _, State=#state{groupid=GroupID}) ->
    case dict:find(UserID, State#state.uid2pid) of
        error ->
            {reply, {error, {{invalid_request, <<"group not found">>}, "user not in a group"}}, State};
        {ok, UserPid} ->
            Event = {message, #message{from=UserID, to={group, GroupID}, body=Message}},
            broadcast(Event, dict:fetch_keys(State#state.pid2uid) -- [UserPid]),
            {reply, ok, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast(_Cast, State) ->
    lager:warning("~p: unhandled cast: ~p", [?MODULE, _Cast]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({'EXIT', OwnerPid, Reason}, State=#state{groupid=GroupID, owner=OwnerPid}) ->
    lager:debug("group '~s': owner ~p died with reason ~p", [GroupID, OwnerPid, Reason]),
    {stop, {shutdown, normal}, State};

handle_info({'EXIT', UserPid, Reason}, State=#state{groupid=GroupID}) ->
    case dict:find(UserPid, State#state.pid2uid) of
        error  -> {noreply, State};
        {ok, UserID} ->
            lager:debug("group '~s': user '~s' exits with reason ~p", [GroupID, UserID, Reason]),
            NewUid2Pid = dict:erase(UserID, State#state.uid2pid),
            NewPid2Uid = dict:erase(UserPid, State#state.pid2uid),
            NewState = State#state{uid2pid=NewUid2Pid, pid2uid=NewPid2Uid},
            Event = {user_left_group, #user_left_group{groupid=GroupID, userid=UserID}},
            broadcast(Event, NewState),
            {noreply, NewState}
    end;

handle_info(_Info, State) ->
    lager:warning("~p: unhandled info: ~p", [?MODULE, _Info]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _) ->
    lager:debug("~p: terminated with reason ~p", [?MODULE, _Reason]).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(Event, #state{pid2uid=Pid2Uid}) ->
    broadcast(Event, dict:fetch_keys(Pid2Uid));

broadcast(Event, PidList) ->
    [ Pid ! {event, Event} || Pid <- PidList ], ok.
