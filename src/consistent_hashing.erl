-module(consistent_hashing).

-include("consistent_hashing.hrl").

-behaviour(gen_server).

%% API
-export([
    add_node/2
    ,remove_node/1
    ,get/1
    ,put/2
    ,delete/1
    ,where_is/1
    ,start/1
    ,stop/0
]).

%% gen_server exports
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
]).

-define(SERVER, ?MODULE).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

add_node(Name, Id) ->
    gen_server:call(?MODULE, {add_node, Name, Id}).

remove_node(Id) ->
    gen_server:call(?MODULE, {remove_node, Id}).

where_is(Key) ->
    gen_server:call(?MODULE, {whereis, Key}).

start(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Nodes], []).

stop() ->
    gen_server:stop(?SERVER).

init([Nodes]) ->
    lists:map(
        fun({RegisterName, Identifier}) ->
            lager:info("Starting server ~p with hash ~p...", [RegisterName, consistent_hashing_lib:hash(Identifier)]),
            supervisor:start_child(consistent_hashing_node_sup, [RegisterName])
        end, Nodes),
    Map = build_map(maps:new(), Nodes),
    {ok, {Map, lists:sort(maps:keys(Map))}}.

handle_call({get, Key}, _From, {HashesToNames, Hashes} = State) ->
    Hash = consistent_hashing_lib:hash(Key),
    NodeIdentifier = successor(Hash, Hashes),
    Node = maps:get(NodeIdentifier, HashesToNames),
    lager:info("Key hash is ~p, should be in node with hash ~p (~p)~n", [Hash, NodeIdentifier, Node]),
    Result = consistent_hashing_node:get(Node, Key),
    {reply, Result, State};

handle_call({put, Key, Value}, _From, {HashesToNames, Hashes} = State) ->
    Hash = consistent_hashing_lib:hash(Key),
    NodeIdentifier = successor(Hash, Hashes),
    Node = maps:get(NodeIdentifier, HashesToNames),
    lager:info("Key hash is ~p, asking node with hash ~p (~p) to store key~n", [Hash, NodeIdentifier, Node]),
    Result = consistent_hashing_node:put(Node, Key, Value),
    {reply, Result, State};

handle_call({delete, Key}, _From, {HashesToNames, Hashes} = State) ->
    Hash = consistent_hashing_lib:hash(Key),
    NodeIdentifier = successor(Hash, Hashes),
    Node = maps:get(NodeIdentifier, HashesToNames),
    lager:info("Key hash is ~p, should be in node with hash ~p (~p)~n", [Hash, NodeIdentifier, Node]),
    Result = consistent_hashing_node:delete(Node, Key),
    {reply, Result, State};

handle_call({add_node, RegisterName, Identifier}, _From, {HashesToNames, Hashes}) ->
    Hash = consistent_hashing_lib:hash(Identifier),
    lager:info("Starting server ~p with hash ~p...", [RegisterName, Hash]),
    supervisor:start_child(consistent_hashing_node_sup, [RegisterName]),
    SuccIdentifier = successor(Hash, Hashes),
    Successor = maps:get(SuccIdentifier, HashesToNames),
    PredecessorId = predecessor(Hash, Hashes),
    % Predecessor = maps:get(SuccIdentifier, PredecessorId),
    lager:info("Next node in ring is ~p (~p)", [SuccIdentifier, Successor]),
    lager:info("Requesting keys inside range (~p, ~p) from ~p (~p)~n",
                [PredecessorId, Hash, SuccIdentifier, Successor]),
    KeysToMigrate = consistent_hashing_node:handoff(Successor, PredecessorId, Hash),
    lager:info("Checking if there are any keys to migrate from node successor..."),
    if
        length(KeysToMigrate) > 0 ->
            lists:map(
                fun({Key, Value}) ->
                    lager:info("Migrating key ~p to server ~p (~p)...~n", [Key, Hash, RegisterName]),
                    consistent_hashing_node:put(RegisterName, Key, Value)
                end, KeysToMigrate);
        true ->
            lager:info("Server doesn't have any keys with lower hash value than ~p, nothing to migrate", [Hash]),
            ok
    end,
    NewState = {maps:put(Hash, RegisterName, HashesToNames), lists:sort([Hash | Hashes])},
    {reply, ok, NewState};

handle_call({remove_node, Identifier}, _From, {HashesToNames, Hashes}) ->
    Hash = consistent_hashing_lib:hash(Identifier),
    RegisteredName = maps:get(Hash, HashesToNames),
    lager:info("Removing server ~p (~p)...~n", [Hash, RegisteredName]),
    SuccIdentifier = successor(Hash, Hashes),
    Successor = maps:get(SuccIdentifier, HashesToNames),
    PredecessorId = predecessor(Hash, Hashes),
    % Predecessor = maps:get(SuccIdentifier, PredecessorId),
    lager:info("Next node in ring is ~p (~p)", [SuccIdentifier, Successor]),
    lager:info("Requesting keys inside range (~p, ~p) from ~p (~p)~n",
                [PredecessorId, SuccIdentifier, SuccIdentifier, Successor]),
    KeysToMigrate = consistent_hashing_node:handoff(RegisteredName, PredecessorId, Hash),
    lager:info("Migrating every key to successor..."),
    lager:info("Getting all keys from server ~p (~p) to be migrated", [Hash, RegisteredName]),
    if
        length(KeysToMigrate) > 0 ->
            lists:map(
                fun({Key, Value}) ->
                    lager:info("Migrating key ~p to server ~p (~p)...~n", [Key, SuccIdentifier, Successor]),
                    consistent_hashing_node:put(Successor, Key, Value)
                end, KeysToMigrate);
        true ->
            lager:info("Server ~p doesn't have any keys in the selected interval, nothing to migrate", [Hash]),
            ok
    end,
    lager:info("Terminating ~p (~p)...~n", [Hash, RegisteredName]),
    supervisor:terminate_child(consistent_hashing_node_sup, Successor),
    NewState = {maps:remove(Hash, HashesToNames), lists:delete(Hash, Hashes)},
    {reply, ok, NewState};

handle_call({whereis, Key}, _From, {HashesToNames, Hashes} = State) ->
    Hash = consistent_hashing_lib:hash(Key),
    NodeIdentifier = successor(Hash, Hashes),
    Node = maps:get(NodeIdentifier, HashesToNames),
    lager:info("Key hash is ~p, should be in node with hash ~p (~p)~n", [Hash, NodeIdentifier, Node]),
    {reply, {NodeIdentifier, Node}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

build_map(Map, []) -> Map;
build_map(Map, [{RegisteredName, Identifier} | T]) ->
    build_map(maps:put(consistent_hashing_lib:hash(Identifier), RegisteredName, Map), T).

successor(Val, L) ->
    successor(Val, L, hd(L)).

successor(_Val, [], Default) -> Default;
successor(Val, [H|_T], _Default) when H > Val -> H;
successor(Val, [_H|T], Default) -> successor(Val, T, Default).

predecessor(Val, L) ->
    successor(Val, lists:reverse(L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

md5_hash_test() ->
    %% note that hashes are ordered in this test, this might be useful to understand what is happening
    %% choose nodes with identifiers that split the ring into more or less even intervals
    ?assertEqual("22A08C584F02D33EDE327FD74D1C63D9", consistent_hashing_lib:hash("node2111@127.0.0.1")),
    ?assertEqual("64824C1EC30E76200A3935D4EFEDCD1D", consistent_hashing_lib:hash("node21@127.0.0.1")),
    ?assertEqual("A12D6508A0C0C064C22FC9A10690B144", consistent_hashing_lib:hash("node6@127.0.0.1")),
    ?assertEqual("E3F21A1267EDD012E71250E5632ABEF5", consistent_hashing_lib:hash("node8@127.0.0.1")),
    %% choose keys that fall nicely between the previous list of servers
    ?assertEqual("1679091C5A880FAF6FB5E6087EB1B2DC", consistent_hashing_lib:hash("6")),
    ?assertEqual("45C48CCE2E2D7FBDEA1AFC51C7C6AD26", consistent_hashing_lib:hash("9")),
    ?assertEqual("6512BD43D9CAA6E02C990B0A82652DCA", consistent_hashing_lib:hash("11")),
    ?assertEqual("98F13708210194C475687BE6106A3B84", consistent_hashing_lib:hash("20")),
    ?assertEqual("A5BFC9E07964F8DDDEB95FC584CD965D", consistent_hashing_lib:hash("37")),
    ?assertEqual("C4CA4238A0B923820DCC509A6F75849B", consistent_hashing_lib:hash("1")),
    ?assertEqual("D3D9446802A44259755D38E6D163E820", consistent_hashing_lib:hash("10")),
    ?assertEqual("ECCBC87E4B5CE2FE28308FD9F2A7BAF3", consistent_hashing_lib:hash("3")),
    ?assertEqual("F7177163C833DFF4B38FC8D2872F1EC6", consistent_hashing_lib:hash("44")).

consistent_hashing_test() ->
    application:set_env(?APP, nodes, [
        {node6, "node6@127.0.0.1"},
        {node8, "node8@127.0.0.1"},
        {node21, "node21@127.0.0.1"}
    ]),
    consistent_hashing_app:start(normal, []),
    ?assert(whereis(?MODULE) =/= undefined),
    ?assert(whereis(node6) =/= undefined),
    ?assert(whereis(node8) =/= undefined),
    ?assert(whereis(node21) =/= undefined),
    ?assert(whereis(node2111) =:= undefined),

    %% basic put and get test, asuring provenance of the key afterwards
    ok = consistent_hashing:put("6", <<"value">>),
    ?assertEqual(<<"value">>, consistent_hashing:get("6")),
    {_, node21} = consistent_hashing:where_is("6"),

    ok = consistent_hashing:put("9", <<"some value">>),
    {_, node21} = consistent_hashing:where_is("9"),

    ok = consistent_hashing:put("11", <<"some value">>),
    {_, node6} = consistent_hashing:where_is("11"),

    ok = consistent_hashing:put("20", <<"some value">>),
    {_, node6} = consistent_hashing:where_is("20"),

    ok = consistent_hashing:put("37", <<"some value">>),
    {_, node8} = consistent_hashing:where_is("37"),

    ok = consistent_hashing:put("1", <<"some value">>),
    {_, node8} = consistent_hashing:where_is("1"),

    ok = consistent_hashing:put("10", <<"some value">>),
    {_, node8} = consistent_hashing:where_is("10"),

    %% wraps around at this point, the 2 following keys belong to a node that is beyond the maximum hash value
    ok = consistent_hashing:put("3", <<"some value">>),
    {_, node21} = consistent_hashing:where_is("3"),

    ok = consistent_hashing:put("44", <<"some value">>),
    {_, node21} = consistent_hashing:where_is("44"),

    %% add new node between node8 and and node21 called node2111
    ok = consistent_hashing:add_node(node2111, "node2111@127.0.0.1"),

    %% see if node 21 assumed the keys that it supposed to takeover
    %% make sure that value is still accessible after key migration
    {_, node2111} = consistent_hashing:where_is("6"),
    ?assertEqual(<<"value">>, consistent_hashing:get("6")),
    {_, node2111} = consistent_hashing:where_is("3"),
    ?assertEqual(<<"some value">>, consistent_hashing:get("3")),
    {_, node2111} = consistent_hashing:where_is("44"),
    ?assertEqual(<<"some value">>, consistent_hashing:get("44")),

    %% remove node 21 that is just before the max hash value.
    ok = consistent_hashing:remove_node("node8@127.0.0.1"),
    {_, node2111} = consistent_hashing:where_is("37"),
    ?assertEqual(<<"some value">>, consistent_hashing:get("37")),
    {_, node2111} = consistent_hashing:where_is("1"),
    ?assertEqual(<<"some value">>, consistent_hashing:get("1")),
    {_, node2111} = consistent_hashing:where_is("10"),
    ?assertEqual(<<"some value">>, consistent_hashing:get("10")),

    ok.

-endif.
