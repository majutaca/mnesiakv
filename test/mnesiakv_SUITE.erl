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

-module(mnesiakv_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [add_category, delete_category,get_category,list_categories,
          add_provider, delete_provider, get_provider, list_providers,
          add_article, list_articles, get_article].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  nhauds:install([node()]),
  application:start(mnesia),
  application:start(mnesiakv),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.


%% @doc Test saving a new document
add_document() ->
  ok.

%% @doc Test retrieving a document by id
get_document() ->
  ok.

%% @doc Test retrieving documents by criteria
get_document_by_criteria() ->
  ok.

%% @doc Test updating a document
update_document() ->
  ok.

%% @doc Test deleting a document
delete_document() ->
  ok.

%% @doc Test id generation
generate_id() ->
  ok.
