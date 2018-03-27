consistent_hashing
=====

Basic implementation of [consistent hashing][1]. The implementation and explanation is a product of my interpretation,
and may be incorrect. If you see that I messed up somewhere in either part and you feel like being cool, please create
an issue or [drop me an email][2].

## Introduction

Consistent Hashing was developed for distributed caching to solve a particular problem: dynamic adding and/or removal of
nodes while minimising the amount of keys that need to be moved around. If by any chance you're wondering how to get
from here to something like [Dynamo][3], just be patient and remember the goal is to build a distributed cache. At this
point it is best not to think about replication or fault tolerance. Note that if a cache server crashes, it is assumed
that you can still fetch the data elsewhere.

This is an OTP application that uses logging to explain you what is happening. If you use default settings, a ring with
4 nodes will be created (each running on an Erlang process). You are able to add and remove servers from the ring by
using the `consistent_hashing` module.

## Limitations

There are many ways to improve this code, and keep in mind that this is a very rudimentary implementation with just some
educational value. No particular attention was given to the efficiency of the data structures and algorithms used. There
is no concern for replication or fault tolerance as well. Implementing replication should be as simple as defining a
rule (replicating factor, `k`) and have a single key be inserted at `k` contiguous nodes in the ring.  
In order to minimise the birthday problem causing nodes with entries very close to each other in the hash interval, one
could use virtual nodes, allowing a single node to register multiple identifiers (e.g. `node1-1@127.0.0.1`,
`node1-2@127.0.0.1`), which would help further distributing the load among the servers. If a server `a` has twice the
capacity as another server `b`, we can register `a` with double the identifiers as `b`.

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell --name shell@consistent_hashing


```erl-sh
15:14:47.966 [info] Application lager started on node shell@consistent_hashing
15:14:47.981 [info] Starting server node1 with hash "14427F20533241D95D4C94AF71FADA9C"...
15:14:47.982 [info] Starting server node2 with hash "8A44B008A23F97B8628B966E30EFCFD0"...
15:14:47.982 [info] Starting server node3 with hash "7676C8E239797C8A69F0EE1889422807"...
15:14:47.982 [info] Starting server node4 with hash "C53C5F838AEBDDB66DE4C6AA4C82E00A"...
15:14:47.982 [info] Starting server node5 with hash "52AC1988EA9BC2E810BBF281F4D64E56"...
15:14:47.982 [info] Starting server node6 with hash "A12D6508A0C0C064C22FC9A10690B144"...
15:14:47.982 [info] Starting server node7 with hash "95E87B8D2512F885EF0F1BDCC3FC541D"...
15:14:47.982 [info] Starting server node8 with hash "E3F21A1267EDD012E71250E5632ABEF5"...
15:14:47.982 [info] Starting server node9 with hash "97F4F378EBDA20F6D0B9915D49DE943F"...
15:14:47.982 [info] Starting server node10 with hash "D254AC8DF316AEFDB557F828101A0D39"...
1> consistent_hashing:get("key").
15:15:15.268 [info] Key hash is "3C6E0B8A9C15224A8228B9A98CA1531D", should be in node with hash "52AC1988EA9BC2E810BBF281F4D64E56" (node5)
miss
2> consistent_hashing:put("key", <<"something">>).
15:15:35.368 [info] Key hash is "3C6E0B8A9C15224A8228B9A98CA1531D", asking node with hash "52AC1988EA9BC2E810BBF281F4D64E56" (node5) to store key
ok
3> consistent_hashing:add_node(node15, "node15identifier").
15:18:36.997 [info] Checking if there are any keys to migrate from node successor...
15:18:36.997 [info] Server doesn't have any keys with lower hash value than "5407FDABA42A5BB4FB2A9AA5A0F5DC9D", nothing to migrate
ok
4> %% Any migration that is necessary will be logged to the console
4> consistent_hashing:remove_node("node15identifier").
ok
5> consistent_hashing:get("key").
15:18:55.167 [info] Key hash is "3C6E0B8A9C15224A8228B9A98CA1531D", should be in node with hash "52AC1988EA9BC2E810BBF281F4D64E56" (node5)
<<"something">>
```

[1]: https://www.akamai.com/es/es/multimedia/documents/technical-publication/consistent-hashing-and-random-trees-distributed-caching-protocols-for-relieving-hot-spots-on-the-world-wide-web-technical-publication.pdf
[2]: mailto:goncalo@goncalotomas.com
[3]: https://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf
