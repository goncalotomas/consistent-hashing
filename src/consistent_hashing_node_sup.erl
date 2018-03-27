%%%-------------------------------------------------------------------
%% @doc consistent_hashing top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(consistent_hashing_node_sup).

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

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{
        id => node,
        start => {consistent_hashing_node, start, []},
        restart => permanent,
        type => worker,
        shutdown => brutal_kill
    }],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
