-module(chat_group_sup).
-behaviour(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0, init/1, start_group/1, start_group/2, start_group/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_group(GroupID) -> start_group(GroupID, undefined, []).
start_group(GroupID, OwnerPid) when is_pid(OwnerPid) -> start_group(GroupID, OwnerPid, []);
start_group(GroupID, Options) when is_list(Options) -> start_group(GroupID, undefined, Options).
start_group(GroupID, OwnerPid, Options) ->
    supervisor:start_child(?MODULE, [GroupID, OwnerPid, Options]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        {chat_group, {chat_group, start_link, []},
            temporary, 60000, worker, [chat_group]}
    ]} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
