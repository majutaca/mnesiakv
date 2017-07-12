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

-module(mnesiakv_server).
-behaviour(gen_server).

-include("document.hrl").

-export([start_link/0, init/1]).

-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3]).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        %% gen_server.

init([]) ->
  	{ok, #state{}}.

handle_call(generate_id, _From, State) ->
  {reply, generate_id(), State};
handle_call({add, #{key:= Key, value := Value}}, _From, State) ->
  Rev = generate_rev(),
  F = fun() ->
    mnesia:write(#document{key=Key, value=Value, rev=Rev})
  end,
  Result = mnesia:activity(transaction, F),
  {reply, {Result, #{key=> Key, value => Value, rev=>Rev}}, State};
handle_call({get, ID}, _From, State) ->
  F = fun() ->
    case mnesia:read({document, ID}) of
            [#document{key=Key, rev=Rev, value=Value}] ->
                {ok, #{key=>Key, rev=>Rev, value=>Value}};
            [] ->
                {undefined, #{}}
        end
    end,
    Result = mnesia:activity(transaction, F),
    {reply, Result, State};
  handle_call({delete, ID}, _From, State) ->
    F = fun() ->
    case mnesia:read({document, ID}) =:= [] of
      true ->
        {undefined, #{}};
      false ->
        Response = mnesia:delete({document, ID}),
        {Response, #{}}
    end
  end,
  Result = mnesia:activity(transaction, F),
  {reply, Result, State}.


handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Utility functions
generate_rev() ->
  binary_to_list(base64:encode(crypto:strong_rand_bytes(20))).

generate_id() ->
  binary_to_list(base64:encode(crypto:strong_rand_bytes(20))).
