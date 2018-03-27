-module(consistent_hashing_node).

-behaviour(gen_server).

%% API
-export ([
    get/2
    ,put/3
    ,delete/2
    ,handoff/3
    ,start/1
    ,stop/1
]).

%% gen_server exports
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
]).

-define(SERVER, ?MODULE).

start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%% Adds an element to the cache
put(Name, Key, Value) ->
    gen_server:call(Name, {put, Key, Value}).

%% Gets the value associated with a certain key
get(Name, Key) ->
    gen_server:call(Name, {get, Key}).

%% Removes an element from the cache
delete(Name, Key) ->
    gen_server:call(Name, {delete, Key}).

%% Returns all keys between a hash range that are held in this node and removes those keys from the state
handoff(Name, From, To) ->
    gen_server:call(Name, {handoff, From, To}).

stop(Name) ->
    gen_server:stop(Name).

%% gen_server callbacks

init(_Args) ->
    {ok, {#{}, []}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({put, Key, Value}, _From, {Map, List}) ->
    {reply, ok, {maps:put(Key, Value, Map), lists:sort([Key | List])}};

handle_call({get, Key}, _From, {Map, _List} = State) ->
    {reply, maps:get(Key, Map, miss), State};

handle_call({delete, Key}, _From, {Map, List}) ->
    {reply, ok, {maps:remove(Key, Map), lists:delete(Key, List)}};

handle_call({handoff, From, To}, _From, {Map, List}) ->
    Keys = maps:keys(Map),
    Fun = case From >= To of
        true ->
            %% wrap around
            fun(K) ->
                H = consistent_hashing_lib:hash(K),
                (H >= consistent_hashing_lib:smallest_hash() andalso H =< To)
                orelse (H >= From andalso H =< consistent_hashing_lib:largest_hash())
            end;
        false ->
            fun(K) ->
                H = consistent_hashing_lib:hash(K),
                H >= From andalso H =< To
            end
    end,
    Filtered = lists:filter(Fun, Keys),
    {reply, maps:to_list(maps:with(Filtered, Map)), {maps:without(Filtered, Map), List}}.
