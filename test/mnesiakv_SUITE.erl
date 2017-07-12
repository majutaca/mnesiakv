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

all() -> [add_document, add_duplicate_document,get_document,get_unavailable_document,
          get_document_by_criteria, update_document, update_document_wrong_rev,
          delete_document, delete_unavailable_document, generate_id].

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
  Key = "test2330l",
  Person = #{"name"=>"Dashiell", "surname"=>"Majuta", "age"=>1},
  {ResponseCode, Document} = mnesiakv:add(#{key=> Key, value=>Person}),

  ?assertEqual(ok, ResponseCode),

  #{key := Key2, rev := Rev, value := Person2} = Document,

  %% Test Returning correct values
  ?assertEqual(Person, Person2),
  ?assertEqual(Key, Key2),

  %% Test revision number add
  ?assert([] =/= Rev).

add_duplicate_document(_Config) ->
  Document = #{key=> "test01", value=>#{"name"=>"Vicky", "surname"=>"Majuta", "age"=>25}},
  Document2 = #{key=> "test01", value=>#{"name"=>"Chris", "surname"=>"Majuta", "age"=>30}},
  {ok, _} = mnesiakv:add(Document),
  {ResponseCode, _} = mnesiakv:add(Document2),
  ?assertEqual(error, ResponseCode).

%% @doc Test retrieving a document by id
get_document(_Config) ->
  %%Gets result if id is found
  Key = "test01",
  {ResponseCode, Document} = mnesiakv:get("test01"),
  ?assertEqual(ok, ResponseCode),

  #{key := Key1, rev := Rev , value := Person} = Document,

  %% Test retreived correct key
  ?assertEqual(Key, Key1),

  %% Test revision number is present
  ?assert([] =/= Rev),

  #{"name" := Name,"surname" := _Surname, "age" := _Age} = Person,
  ?assertEqual("Vicky", Name).

%% Get document that is not available
get_unavailable_document(_Config) ->
  %% Test returns undefined if id is not found
  {ResponseCode, _Doc} = mnesiakv:get("some_id"),
  ?assertEqual(undefined, ResponseCode).

%% @doc Test retrieving documents by criteria
get_document_by_criteria(_Config) ->
  ok.

%% @doc Test updating a document
update_document(_Config) ->
  {ResponseCode, Document} = mnesiakv:get("test01"),
  ?assertEqual(ok, ResponseCode),

  #{key := Key, rev := Rev , value := Person} = Document,

  Person2 = maps:update("age",25,Person),
  Document2 = maps:update(value, Person2, Document),
  {ResponseCode, _} = mnesiakv:update(Document2),
  ?assertEqual(ok, ResponseCode),

  {_, Document3} = mnesiakv:get("test01"),
  #{key := Key3, rev := Rev3 , value := Person3} = Document3,
  #{"name" := Name,"surname" := _Surname, "age" := Age} = Person3,
  ?assertEqual("Vicky", Name), %% Name has not changed
  ?assertEqual(Key, Key3), %% Key has not changed
  ?assertEqual(25, Age), %% Age has changed
  ?assert(Rev =/= Rev3).

%% @doc Test updating a document with no or wrong revision
update_document_wrong_rev(_Config) ->
  Document = #{key=> "test01", value=>#{"name"=>"Chris", "surname"=>"Majuta", "age"=>1}, rev=>"SomeRev"},
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
  {ResponseCode, _} = mnesiakv:delete("not_id"),
  ?assertEqual(undefined, ResponseCode).

%% @doc Test id generation
generate_id(_Config) ->
  Id = mnesiakv:generate_id(),
  ?assert("" =/= Id),
  ?assert(0 =/= string:len(Id)).
