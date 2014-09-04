-module(tests).

-export([test1/0,test2/0,test3/0,test4/0,test5/0,test6/0,test7/0,test8/0,test9/0,test10/0,test11/0]).

%%%-------------------------------------------------------------------------------------------
%%% Test1 : Testing the at_server start function for starting an new atomic transaction Server
%%%-------------------------------------------------------------------------------------------
test1() ->
	at_server:start(4).

%%%------------------------------------------------------------------------------------------
%%% Test2 : Testing the at_server begin_t function 
%%%------------------------------------------------------------------------------------------
test2() ->
	{ok,AT} = at_server:start([]),
	at_server:begin_t(AT).

%%%------------------------------------------------------------------------------------------
%%% Test3 : Testing the at_server start,begin_t,update_t, and doquery functions 
%%%------------------------------------------------------------------------------------------
test3() ->
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF3} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF3, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	GetState =
		fun(X) ->
			X
		end,
	at_server:doquery(AT, GetState).

%%%------------------------------------------------------------------------------------------
%%% Test4 : Testing the at_server start,begin_t,update_t, and query_t functions 
%%%------------------------------------------------------------------------------------------
test4() ->
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF3} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF3, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	GetState =
		fun(X) ->
			X
		end,
	at_server:query_t(AT,REF2,GetState).

%%%------------------------------------------------------------------------------------------
%%% Test5 : Testing the at_server commit_t function 
%%%------------------------------------------------------------------------------------------
test5() ->
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF3} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF3, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:commit_t(AT, REF2).


%%%------------------------------------------------------------------------------------------
%%% Test6 : Testing the at_server aborted commit and the stop function 
%%%------------------------------------------------------------------------------------------
test6() ->	
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF3} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:commit_t(AT, REF2),
	at_server:update_t(AT, REF3, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:commit_t(AT, REF2), 
	at_server:stop(AT).

%%%------------------------------------------------------------------------------------------
%%%  
%%%------------------------------------------------------------------------------------------
test7() ->	
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	at_extapi:abort(AT, REF2),
	GetState =
		fun(X) ->
			X
		end,
	at_server:query_t(AT,REF2,GetState).

%%%------------------------------------------------------------------------------------------
%%% Test8 : Testing the at_extapi tryUpdate function  
%%%------------------------------------------------------------------------------------------
test8() ->	
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	spawn(fun() -> at_extapi:tryUpdate(AT, Function) end),
	timer:sleep(200),	
	at_server:commit_t(AT, REF2),
	timer:sleep(2000),	
	GetState =
		fun(X) ->
			X
		end,
	at_server:doquery(AT, GetState).	

%%%------------------------------------------------------------------------------------------
%%% Test9 : Testing the at_extapi ensureUpdate function  
%%%------------------------------------------------------------------------------------------
test9() ->	
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	spawn(fun() -> at_extapi:ensureUpdate(AT, Function) end),
	timer:sleep(200),	
	at_server:commit_t(AT, REF2),
	timer:sleep(2000),	
	GetState =
		fun(X) ->
			X
		end,
	at_server:doquery(AT, GetState).

%%%------------------------------------------------------------------------------------------
%%% Test10 : Testing the at_extapi ensureUpdate2 function  
%%%------------------------------------------------------------------------------------------
test10() ->	
	{ok,AT} = at_server:start(4),
	Function = 
		fun(X) -> 
			X+5
		end,
	{ok,REF1} = at_server:begin_t(AT),
	{ok,REF2} = at_server:begin_t(AT),
	{ok,REF4} = at_server:begin_t(AT),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF1, Function),
	at_server:update_t(AT, REF4, Function),
	at_server:update_t(AT, REF2, Function),
	at_server:update_t(AT, REF2, Function),
	spawn(fun() -> at_extapi:ensureUpdate2(AT, Function) end),
	timer:sleep(200),	
	at_server:commit_t(AT, REF2),
	timer:sleep(2000),	
	GetState =
		fun(X) ->
			X
		end,
	at_server:doquery(AT, GetState).

%%%--------------------------------------------------------------------------------
%%% Test11 : Testing the module's at_extapi choiceUpdate function 
%%%--------------------------------------------------------------------------------

test11() ->
	{ok,AT} = at_server:start(10),
	DoubleFun = 
		fun(X,E) -> 
			X + 2 +  E
		end,
	at_extapi:choiceUpdate(AT, DoubleFun, [11,2,54,3]),
	timer:sleep(2000),
	GetState =
		fun(X) ->
			X
		end,
	at_server:doquery(AT, GetState).