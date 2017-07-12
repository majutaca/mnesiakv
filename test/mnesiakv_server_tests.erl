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

-module(mnesiakv_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/document.hrl").

setup() ->
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = mnesiakv_server:start_link(),
    Pid.

cleanup(Pid) ->
  ?debugMsg("cleanup"),
  exit(Pid, kill), %% brutal kill!
  ?assertEqual(false, is_process_alive(Pid)).

mnesiakv_server_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun add_document/1,
        fun get_document/1,
        fun delete_document/1,
        fun generate_id/1
    ]}.

server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.

add_document(_Pid) ->
  fun() ->
    meck:new(mnesia,[non_strict]),
    meck:expect(mnesia, activity, fun(transaction, _F) -> ok end),

    Key = "test2330l",
    Person = #{"name"=>"Dashiell", "surname"=>"Majuta", "age"=>1},
    {Result, Document} = gen_server:call(mnesiakv_server, {add, #{key=> Key,
                    value=>Person}}),
    ?assertEqual(ok, Result),

    #{key := Key2, rev := Rev, value := Person2} = Document,

    %% Test Returning correct values
    ?assertEqual(Person, Person2),
    ?assertEqual(Key, Key2),

    %% Test revision number add
    ?assert([] =/= Rev),

    meck:unload(mnesia)
  end.

get_document(_Pid) ->
  fun() ->

    Key = "test2330l",
    Rev = "847566eryhjdyhdye7747",
    Person = #{"name"=>"Dashiell", "surname"=>"Majuta", "age"=>1},
    %%Document = #{key=>Key, rev=>Rev, value=>Person},

    meck:new(mnesia,[non_strict]),
    meck:expect(mnesia, activity, fun(transaction, F) -> F() end),
    meck:expect(mnesia, read, fun({document, ID}) ->
      case ID of
        Key ->
          [#document{key=Key, rev=Rev, value=Person}];
        _ ->
          []
        end
    end),

    {Result, Document} = gen_server:call(mnesiakv_server, {get, Key}),
    ?assertEqual(ok, Result),

    #{key := Key2, rev := Rev2, value := Person2} = Document,

    %% Test Returning correct values
    ?assertEqual(Person, Person2),
    ?assertEqual(Key, Key2),
    ?assertEqual(Rev, Rev2),

    {Result2, Document2} = gen_server:call(mnesiakv_server, {get, "SomeKey"}),
    ?assertEqual(undefined, Result2),
    ?assertEqual(#{}, Document2),

    meck:unload(mnesia)
  end.

delete_document(_Pid) ->
  fun() ->

    Key = "test2330l",
    Rev = "847566eryhjdyhdye7747",
    Person = #{"name"=>"Dashiell", "surname"=>"Majuta", "age"=>1},
    %%Document = #{key=>Key, rev=>Rev, value=>Person},

    meck:new(mnesia,[non_strict]),
    meck:expect(mnesia, activity, fun(transaction, F) -> F() end),
    meck:expect(mnesia, read, fun({document, ID}) ->
      case ID of
        Key ->
          [#document{key=Key, rev=Rev, value=Person}];
        _ ->
          []
        end
    end),

    {Result, Document} = gen_server:call(mnesiakv_server, {get, Key}),
    ?assertEqual(ok, Result),

    #{key := Key2, rev := Rev2, value := Person2} = Document,

    %% Test Returning correct values
    ?assertEqual(Person, Person2),
    ?assertEqual(Key, Key2),
    ?assertEqual(Rev, Rev2),

    {Result2, Document2} = gen_server:call(mnesiakv_server, {get, "SomeKey"}),
    ?assertEqual(undefined, Result2),
    ?assertEqual(#{}, Document2),

    meck:unload(mnesia)
  end.


generate_id(_Pid) ->
  fun() ->
    Id = gen_server:call(mnesiakv_server, generate_id),
    ?debugMsg("GENERATED ID: "++Id),
    ?assert("" =/= Id),
    ?assert(0 =/= string:len(Id))
  end.
