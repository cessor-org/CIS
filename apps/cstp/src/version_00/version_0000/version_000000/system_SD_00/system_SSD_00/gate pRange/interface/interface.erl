%%%-------------------------------------------------------------------
%% @doc
%%  Cessor Information Systems, CIS
%%  CIS License v0.1.0
%%  https://cessor.org
%%  Cessor Service Transaction Protocol
%%  Version Crown-Block index = 0
%%  Version Serial Domain = 0
%%  Version Serial Number = 0
%%  Service Domain = 0;         System service domain
%%  Service Sub-Domain = 0;     System service sub-domain
%%  Procedure Range = 3;        Gate Procedure Packages Range
%%  Procedure Package = 0;      Interface Procedure Package
%%  Process Module = 0;         State Process Module
%%  Process Module = 1;         Call Process Module
%%%%%%%%%%%% Interface Generic Server %%%%%%%%%%%%
%% @end
%%%-------------------------------------------------------------------
-module(interface).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%  START gen_server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%  INIT
init([]) ->
    %process_flag(trap_exit, true),
    
    % Service status
    STATUS = init,  % init, ready, challenge, problem
    % Gate services
    Gate_services = [
        {port,[], unset}
    ],
    % Reference list of asynchronous requests
    Reference_list = [],
    % System state
    STATE =
        #{
            status => STATUS
            , service => Gate_services
            , proc_ref=>Reference_list
            %, caller=>Caller   % temporary used on call events
        },
    case register(interface, self()) of
        true ->
            {ok, STATE};
        _->
            {stop, _reason=register}
    end.

%%  CALL
handle_call(
    CSTX =
        <<
            0,0,0   %version
            ,0,0    %system services
            ,3,0    %gate pRange, interface pPack
            ,1      %call module
            ,_/binary>>,
    {Caller, _tag},
    STATE) ->
    Call_info = #{
        caller=>Caller
        },
    Merge_state=maps:merge(Call_info, STATE),
    case cstp:compute(CSTX, Merge_state) of
        {ok, Reply, New_state} ->
            STATE_clear = maps:remove(caller, New_state),
            {reply, Reply, STATE_clear};
        {error, Reason} ->
            ft({error, [call,cstp]++Reason}),
            {reply, {error, Reason}, STATE}
    end;
handle_call(_, Caller, STATE) ->
    ft({error, [call, Caller]}),
    {noreply, STATE}.

%%  CAST
handle_cast({Ref, CSTX}, STATE) ->
    Process_refs = maps:get(proc_ref, STATE),
    case lists:member(Ref, Process_refs) of
        true ->
            Ref_list = lists:delete(Ref, Process_refs),
            STATE_clear = maps:update(proc_ref, Ref_list, STATE),
            case cstp:compute(CSTX, STATE_clear) of
                {ok, NewState} ->
                    {noreply, NewState};
                {stop,Reason,NewState} ->
                    {stop,Reason,NewState};
                {error, Reason} ->
                    ft(Reason),
                    {noreply, STATE_clear}
            end;
        false ->
            ft({error, [cast,ref]}),
            {noreply, STATE}
    end;
handle_cast(CAST, STATE) ->
    ft({error, [cast, CAST]}),
    {noreply, STATE}.

%%  INFO
%%  Debug
handle_info(debug, STATE) ->
    log({debug, STATE}),
    {noreply, STATE};
handle_info(INFO, STATE) ->
    ft({error, [info, INFO]}),
    {noreply, STATE}.

%%  TERMINATE
terminate(_Reason, _STATE) ->
    ok.

%%  CODE_CHANGE
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.

%%  BIFs
log(MSG) ->
    io:format("gate| ~p~n", [MSG]).
%%% Fault tolerence
ft(FT)->
    log(["FT got ", FT]).