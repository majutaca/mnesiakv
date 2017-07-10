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

-include("../src/document.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [add_category, delete_category,get_category,list_categories,
          add_provider, delete_provider, get_provider, list_providers,
          add_article, list_articles, get_article].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  mnesiakv:install([node()]),
  application:start(mnesia),
  application:start(mnesiakv),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

init_per_testcase(_, Config) ->
  F = fun() ->
      mnesia:write(#document{key = "test01", value=#{"name"=>"Vicky", "surname"=>"Majuta", "age"=>1}})
  end,
  mnesia:activity(transaction, F),
  Config.

end_per_testcase(_, _Config) ->
  ok.


%% @doc Test saving a new document
add_document(_Config) ->
  Person = #{key=> "test2330l", value=>#{"name"=>"Dashiell", "surname"=>"Majuta", "age"=>1}},
  {ResponseCode, Document} = mnesiakv:add(Person),

  ?assertEqual(ok, ResponseCode),

  Person2 = map:find(value, Document),
  %% Test Returning correct values
  {ok, Name} = map:find("name", Person2),
  {ok, Age} = map:find("age", Person2),
  ?assertEqual("Dashiell", Name),
  ?assertEqual(1, Age),

  %% Test revision number add
  {ok, Revision} = map:find("rev", Person2),
  ?assert([] =/= Revision).

add_duplicate_document(_Config) ->
  Person = #{key=> "test01", value=>#{"name"=>"Vicky", "surname"=>"Majuta", "age"=>1}},
  {ResponseCode, Person} = mnesiakv:add(Person),
  ?assertEqual(error, ResponseCode).

%% @doc Test retrieving a document by id
get_document(_Config) ->
  %%Gets result if id is found
  {ResponseCode, Document} = mnesiakv:get("test01"),
  ?assertEqual(ok, ResponseCode),

  {ok, Person} = map:find(value, Document),
  ?assert(#{} =/= Person),

  {ok, Name} = map:find("name", Person),
  ?assertEqual("Vicky", Name).

%% Get document that is not available
get_unavailable_document(_Config) ->
  %% Test returns undefined if id is not found
  {ResponseCode, _Id} = mnesiakv:get("some_id"),
  ?assertEqual(undefined, ResponseCode).

%% @doc Test retrieving documents by criteria
get_document_by_criteria(_Config) ->
  ok.

%% @doc Test updating a document
update_document(_Config) ->
  {ResponseCode, Document} = mnesiakv:get("test01"),
  ?assertEqual(ok, ResponseCode),

  {ok, Person} = map:find(value, Document),
  ?assert(#{} =/= Person),

  Person2 = maps:update("age",25,Person),
  Document2 = maps:update(value, Person2, Document),
  {ResponseCode, Document3} = mnesiakv:update(Document2),
  ?assertEqual(ok, ResponseCode),

  {ok, Person3} = map:find(value, Document3),
  {ok, Name} = map:find("name", Person3),
  ?assertEqual("Vicky", Name), %% Name has not changed

  {ok, Age} = map:find("age", Person3),
  ?assertEqual(25, Age),

  %% Assert Revision has changed
  {ok, Rev1} = map:find("name", Person),
  {ok, Rev2} = map:find("name", Person3),
  ?assert(Rev1 =/= Rev2).

%% @doc Test updating a document with no or wrong revision
update_document_wrong_rev(_Config) ->
  Document = #{key=> "test01", value=>#{"name"=>"Chris", "surname"=>"Majuta", "age"=>1}},
  {ResponseCode, _Document} = mnesiakv:update(Document),
  ?assertEqual(optimistic_lock, ResponseCode).

%% @doc Test deleting a document
delete_document(_Config) ->
  {ResponseCode, _} = mnesiakv:delete("test01"),
  ?assertEqual(ok, ResponseCode),

  {ResponseCode2, _Document} = mnesiakv:get("test01"),

  ?assertEqual(undefined, ResponseCode2).

%% @doc Test deleting a document with no or wrong re
delete_unavailable_document(_Config) ->
  {ResponseCode, _} = mnesiakv:delete("test01"),
  ?assertEqual(undefined, ResponseCode).

%% @doc Test id generation
generate_id(_Config) ->
  Id = mnesiakv:generate_id(),
  ?assert("" =/= Id),
  ?assert(0 =/= string:len(Id)).
