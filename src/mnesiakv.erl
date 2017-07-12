%% Copyright (c) 2017, Christian A Majuta <majutaca@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mnesiakv).

-export([install/1, generate_id/0, add/1, get/1, delete/1]).

-include("document.hrl").

install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(document,
                        [{attributes, record_info(fields, document)},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).


%% @doc Function for generating unique document IDs
-spec generate_id() -> string().
generate_id() ->
  gen_server:call(mnesiakv_server, generate_id).

%% @doc Function for creating new documents
-spec add(map()) -> {atom(), map()}.
add(Document) ->
  gen_server:call(mnesiakv_server, {add, Document}).

%% @doc Function for getting a document by id
-spec get(string()) -> {atom(), map()}.
get(ID) ->
  gen_server:call(mnesiakv_server, {get, ID}).

%% @doc Function for deleting a document by id
-spec delete(string()) -> {atom(), map()}.
delete(ID) ->
  gen_server:call(mnesiakv_server, {delete, ID}).
