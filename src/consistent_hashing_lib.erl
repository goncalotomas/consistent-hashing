-module(consistent_hashing_lib).

-export([
    hash/1
    ,largest_hash/0
    ,smallest_hash/0
]).

hash(Something) ->
    hex_hash(md5, Something).

%% Use only the first 128 bits of the SHA1 hash
hex_hash(sha1, Something) ->
    <<X:128, _:32>> = crypto:hash(sha, Something),
    integer_to_list(X, 16);

%% Use the whole MD5 hash as a checksum
hex_hash(md5, Something) ->
    <<X:128>> = crypto:hash(md5, Something),
    integer_to_list(X, 16).

largest_hash() ->
    largest_hash(md5).

largest_hash(md5) ->
    string:copies("F", 32).

%% Hashes are being represented in strings, and the empty string evaluates to lower than all possible hash values
smallest_hash() ->
    smallest_hash(md5).

smallest_hash(md5) ->
    "".
