%%%-------------------------------------------------------------------
%% @doc consistent_hashing top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(consistent_hashing_sup).

-include("consistent_hashing.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    {ok, Nodes} = application:get_env(?APP, nodes),
    ChildSpecs = [
        #{
            id => node_sup,
            start => {consistent_hashing_node_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => app,
            start => {consistent_hashing, start, [Nodes]},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
