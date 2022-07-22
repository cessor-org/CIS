%%%-------------------------------------------------------------------
%% @doc
%%  https://cessor.org
%%  Cessor Information Systems, CIS
%%  CIS License v0.1.0
%%  Cessor Service Transaction Protocol
%%  Version Crown-Block index = 0
%%  Version Serial Domain = 0
%%  Version Serial Number = 0
%%  Service Domain = 0;         System service domain
%%  Service Sub-Domain = 0;     System service sub-domain
%%  Procedure Range = 0;        Unit Procedure Packages Range
%%  Procedure Package = 0;      Core Procedure Package
%%  Process Module = 0;         State Process Module
%%  Process Module = 1;         Call Process Module
%%%%%%%%%%%% Core Generic Server %%%%%%%%%%%%
%% @end
%%%-------------------------------------------------------------------
-module(core).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%  START gen_server
start_link(INIT_STATE) ->
    gen_server:start_link(?MODULE, [INIT_STATE], []).

%%  INIT
init([INIT_STATE]) ->
    case maps:find(pin, INIT_STATE) of
        {ok, PIN} ->
            TX = [
                _version={0,0,0},
                _service={0,0},     %system services
                _procedure={0,0},   %unit pRange, core_man pPack
                _process={0,1}      %state pMod, boot pRef
            ],
            {ok, CSTX} = cstp:compose(TX, #{pin=>PIN}),
            log({cstx, CSTX}),
            Ref = make_ref(),
            gen_server:cast(self(), {Ref, CSTX}),

            % System status
            STATUS = unset,  % unset, init, boot, ready
            % System services
            SYS_services = [
                {crypto, [], unset},
                {store, [], unset},
                {gate, [], unset}
            ],
            % Reference list of asynchronous requests
            Reference_list = [Ref],
            % System state
            STATE = #{
                status => STATUS
                , service => SYS_services
                , proc_ref=>Reference_list
                %, caller=>Caller   % temporary used on call events
            },
            case register(core, self()) of
                true ->
                    {ok, STATE};
                _->
                    {stop, _reason=register}
            end;
        _->
            {stop,[pin]}
    end.

%%  CALL
handle_call(
    CSTX =
        <<
            0,0,0   %version
            ,0,0    %system services
            ,0,0    %unit pRange, core_man pPack
            ,1      %call p_Mod
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
            ft({error, [core_call,cstp]++Reason}),
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
            ft({error, [cast, {Ref, CSTX}]}),
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
    io:format("core| ~p~n", [MSG]).
%%% Fault tolerence
ft(FT)->
    log(["FT got ", FT]).