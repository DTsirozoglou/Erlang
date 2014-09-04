%%%-------------------------------------------------------------------
%%% @author Michael Kirkedal Thomsen <shapper@diku.dk>
%%% @copyright (C) 2013, Michael Kirkedal Thomsen
%%% @doc
%%% Skeleton for AP Exam 2013.
%%% Implementation of the atomic transaction server
%%% @end
%%% Created : Oct 2013 by Michael Kirkedal Thomsen <shapper@diku.dk>
%%%-------------------------------------------------------------------
%%% Student name: Dimitrios Tsirozoglou
%%% Student KU-id: kjg920
%%%-------------------------------------------------------------------

-module(at_extapi).

-export([abort/2, tryUpdate/2, ensureUpdate/2,  choiceUpdate/3,ensureUpdate2/2]).

%%%-------------------------------------------------------------------
%%% Extended API
%%%-------------------------------------------------------------------

abort(AT, Ref) -> 
					at_server:update_t(AT, Ref, error).

tryUpdate(AT, Fun) -> 
					try at_server:doquery(AT, Fun) of
                            {ok,_} ->
                               {ok,Ref} = at_server:begin_t(AT),
                               timer:sleep(500),
							   at_server:update_t(AT, Ref, Fun),
							   case at_server:commit_t(AT, Ref) of
							   		aborted -> 
							   					aborted;
							   		ok ->       io:format(" Done "),
							   					ok
							   	end;
							error ->
								error
                        catch
                            _:_->
                                error
                        end.

ensureUpdate2(AT, Fun) ->
					try tryUpdate(AT, Fun) of
                            ok -> 
                            		ok;
                            aborted ->
                            		ensureUpdate2(AT, Fun);
                            error ->
								error
                        catch
                            _:_->
                                error
                        end.

ensureUpdate(AT, Fun) ->
					{ok, UpdatedState} = at_server:doquery(AT, Fun),
					HelperServer = spawn(fun() -> helperServer(AT, Fun, []) end),
				    Response = rpc(HelperServer, ensureUpdate),
				    case Response of
				    	ok ->
				    			AT,
				    			io:write(AT);
				    	aborted ->
				    			UpdServer=
				    					fun(_) ->
				    						UpdatedState
				    					end,
				    			ensureUpdate(AT, UpdServer);
				    	error -> 
				    			error
				    		end.

choiceUpdate(AT, Fun, Val_list) -> 
					HelperServer = spawn(fun() -> helperServer(AT, Fun, Val_list) end),
				    rpc(HelperServer, choiceUpdate).
		
%%%-------------------------------------------------------------------
%%% Communication primitives
%%%-------------------------------------------------------------------

%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
    {Pid, Response} ->
        Response
    end.

rpc2([], {choiceUpdate,_}) ->
	receive
		    {_, Response} ->
		        Response
    end;	

rpc2([Pid|Rest], {choiceUpdate,[Fun|Funs]}) ->
    Pid ! {self(), {choiceUpdate,Fun}},
    rpc2(Rest,{choiceUpdate,Funs}).

reply(From, Msg) ->
    From ! {self(), Msg}.


%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Implementation of the helper Server

helperServer(AT, Fun, Val_list) ->
		receive 

			{From, ensureUpdate} ->
							Result = tryUpdate(AT, Fun),
							reply(From, Result);
			{From, choiceUpdate} ->
							GetState = 
										fun(X) -> 
											X 
										end,
							{ok, State} = at_server:doquery(AT, GetState),
							Pids = createHelpTrans(AT,Val_list,[]),
							HelpFuns = functions(State, Val_list, Fun, []),
							io:write(Pids),
							io:write(HelpFuns),
							Response = rpc2(Pids, {choiceUpdate,HelpFuns}),
							reply(From, Response)
		end.

% Implementation of the helper Server

chooseUpd(AT) ->
		receive
			{From, {choiceUpdate,Fun}} ->
							Result = tryUpdate(AT, Fun),
							reply(From, Result)
		end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

createHelpTrans(_,[],Pids) -> Pids;
createHelpTrans(AT, [Val|List],List1) ->
	Pid = spawn(fun() -> chooseUpd(AT) end),
	io:write(Val),
	createHelpTrans(AT,List,[Pid|List1]).


functions(_, [], _, NewFuns) -> NewFuns;
functions(State, [Val|List], Fun,NewFuns) ->
	HelpFun = 
			fun(_) ->
				 Fun(State,Val)
			end,
	functions(State, List, Fun,[HelpFun|NewFuns]).			

