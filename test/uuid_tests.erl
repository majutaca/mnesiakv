-module(uuid_tests).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
  Id = mnesiakv:generate_id(),
  ?assert("" =/= Id),
  ?assert(0 =/= string:len(Id)).
