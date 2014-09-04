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

-module(at_server).

-export([start/1, stop/1, begin_t/1, doquery/2, query_t/3, update_t/3, commit_t/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start (State) ->
    {ok,spawn(fun() -> atServer(State,dict:new()) end)}.

stop(AT) -> rpc(AT, stop).

doquery(AT, Fun) -> rpc(AT, {doquery, Fun}).

update_t(AT, Ref, Fun) -> info(AT,{update_t,Ref, Fun}).

% Returns a reference

begin_t(AT) ->rpc(AT, begin_t).

query_t(AT, Ref, Fun) -> rpc(AT,{query_t, Ref, Fun}).

commit_t(AT, Ref) -> rpc(AT,{commit_t, Ref}).

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

reply(From, Msg) ->
    From ! {self(), Msg}.


reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

reply_error(From) ->
    reply(From, error).

reply_abort(From) ->
    reply(From, aborted).

%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

%%%-------------------------------------------------------------------
%%% Internal Implementation
%%%-------------------------------------------------------------------

% Implementation of the atomic transaction server.

atServer(State,Dictionary) ->
    receive

        {From,begin_t} ->
            Ref = make_ref(),
            Condition = running,
            Transact = spawn(fun() ->trans(State)end),
            io:format(" ProcessCreated !"),
            Newdict = dict:store(Ref, {Transact, Condition}, Dictionary),
            reply(From,{ok,Ref}),
            atServer(State,Newdict);

        {From ,{query_t, Ref, Fun}} ->
            try Dictionary:fetch(Ref) of
                    Result->
                            case Result of  
                                {Pid,running} ->
                                        Response = rpc(Pid,{query_t, Fun}),
                                        case Response of
                                            aborted ->
                                                Newdict = Dictionary:store(Ref,{Pid,aborted}),
                                                reply(From,Response),
                                                atServer(State,Newdict);
                                            _->         
                                                reply(From,Response),
                                                atServer(State,Dictionary)
                                        end;
                                {_,aborted} -> 
                                        reply_abort(From),
                                        atServer(State,Dictionary)
                            end
                catch
                    _:_->
                        reply_error(From),
                        atServer(State,Dictionary)
                end;

        {update_t,Ref, Fun} ->
            try Dictionary:fetch(Ref) of
                     Result->
                            case Result of  
                                {Pid,running} ->
                                        Response = rpc(Pid,{update_t,Fun}),
                                        case Response of
                                            aborted ->
                                                Newdict = Dictionary:store(Ref,{Pid,aborted}),
                                                atServer(State,Newdict);
                                            _->         
                                                atServer(State,Dictionary)
                                        end;
                                {_,aborted} -> 
                                        atServer(State,Dictionary)
                            end
                catch
                    _:_->
                        atServer(State,Dictionary)
                end;
                     
        {From,{commit_t, Ref}} ->
                    try Dictionary:fetch(Ref) of
                            Result ->
                                    case Result of  
                                        {Pid,running} ->
                                                {ok,NewStatte} = rpc(Pid,commit_t),
                                                Refs = Dictionary:fetch_keys(),
                                                NewDictionary = dictUpdate(Dictionary,Refs),
                                                io:format(" Newdict "),
                                                reply_ok(From),
                                                atServer(NewStatte,NewDictionary);
                                        {_,aborted} ->
                                                io:format(" Aborded "),
                                                reply_abort(From),
                                                atServer(State,Dictionary)
                                    end
                        catch
                            _:_->
                                reply_error(From),
                                atServer(State,Dictionary)
                        end;

        {From, {doquery, Fun}} ->
                    try Fun(State) of
                            Results ->
                                reply(From,{ok,Results})
                        catch
                            _:_->
                                reply_error(From)
                        after
                            atServer(State,Dictionary)
                        end;

        {From, stop} ->
                    reply_ok(From,State),
                    Refs = Dictionary:fetch_keys(),
                    dictUpdate(Dictionary,Refs),
                    exit(self(), "Terminated!")
    end.

% Implementation of the atomic transactions helper function.

trans(State) -> 
    receive
        {From,commit_t} ->
                    reply(From,{ok,State});                    

        {From,{update_t, Fun}} ->
                    try Fun(State) of
                                Results ->
                                    reply(From,{ok,Results}),
                                    trans(Results)
                            catch
                                _:_->
                                   reply_abort(From),
                                   exit(self(), "Terminated!")
                            end;                                

        {From ,{query_t, Fun}} ->
                    try Fun(State) of
                                Results ->
                                    reply(From,{ok,Results})
                            catch
                                _:_->
                                   reply_abort(From),
                                   exit(self(), "Terminated!")
                            end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictUpdate(Dictionary,[]) -> Dictionary;
dictUpdate(Dictionary,[Ref1|Rest]) ->
                    {Pid,_} = dict:fetch(Ref1, Dictionary),
                    Dictionary2 = dict:store(Ref1,{Pid,aborted}, Dictionary),
                    exit(Pid, "Terminated!"),
                    dictUpdate(Dictionary2,Rest).

