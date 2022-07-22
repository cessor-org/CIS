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
%%  Procedure Package = 2;      Interface_edge package
%%  Process Module = 0;         State Process Module
%%  Process Module = 1;         Call Process Module
%%%%%%%%%%%% Interface_edge Generic Server %%%%%%%%%%%%
%% @end
%%%-------------------------------------------------------------------
-module(interface_edge).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%  START gen_server
start_link(ARGS) ->
    gen_server:start_link(?MODULE, ARGS, []).

%%  INIT
init(ARGS) ->
    %process_flag(trap_exit, true),
    TX = [
        _version={0,0,0},
        _service={0,0},     %system services
        _procedure={3,2},   %Gate pRange, interface edge pPack
        _process={0,1}      %state pMod, boot pRef
    ],
    {ok, CSTX} = cstp:compose(TX, #{}),
    Ref = make_ref(),
    gen_server:cast(self(), {Ref, CSTX}),
    {port, Port} = lists:keyfind(port, 1, ARGS),
    STATE =
        #{
            status => init  % boot, ready, challenge, problem
            , proc_ref=>[Ref]
            , port=>Port
            %, caller=>Caller   % temporary used on call events
        },
    {ok, STATE}.

%%  CALL
handle_call(
    CSTX =
        <<
            0,0,0   %version
            ,0,0    %system services
            ,3,2    %gate pRange, interface edge pPack
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
    {noreply, STATE};
handle_cast({accept, LSocket}, STATE) ->
    case {accept, LSocket} of
        {ok, Socket}->
            interface_server ! {{client, self()}, connect},
            Status_STATE = maps:update(status, connect, STATE),
            New_STATE = maps:update(socket, Socket, Status_STATE),
            {noreply, New_STATE};
        {error, Reason}->
            interface_server ! {{client, self()}, {error, Reason}},
            {stop, normal, STATE}
        end;
handle_cast({send, _CSTX}, STATE) ->
    {noreply, STATE};
handle_cast({call_back, TX}, STATE) ->
    case cstp:compose(TX, STATE) of
        {ok, CSTX} ->
            gen_server:cast(self(), {send, CSTX});
        {error, Reason} ->
            %%  ft ! {error, Reason},
            {error, Reason}
    end,
    {noreply, STATE};
handle_cast(CAST, STATE) ->
    log(["Cast unexpected", CAST]),
    {noreply, STATE}.

%%  INFO
handle_info({ssl, SOCKET, CSTX}, STATE) ->
    case CSTX of
        <<
            0,0,0   %version
            ,0,0    %system services
            ,3,2    %gate pRange, interface edge pPack
            ,2      %info module
            ,_/binary
        >> ->
            Call_info = #{
                caller=>SOCKET
            },
            Merge_state=maps:merge(Call_info, STATE),
            case cstp:compute(CSTX, Merge_state) of
                {ok, New_state} ->
                    STATE_clear = maps:remove(caller, New_state),
                    {ok, STATE_clear};
                {stop,Reason,NewState} ->
                    {stop,Reason,NewState};
                {error, Reason} ->
                    ft({error, [info,cstp]++Reason}),
                    {noreply, STATE}
            end;
        _ ->
            ft({error, [info, cstx]}),
            {noreply, STATE}
    end;
handle_info({ssl_closed, SOCKET}, STATE) ->
    TX = [
        _version={0,0,0},
        _service={0,0},     %system services
        _procedure={3,2},   %gate pRange, interface edge pPack
        _process={2,254}      %info pMod, ssl_closed pRef
    ],
    {ok, CSTX} = cstp:compose(TX, STATE),
    Call_info = #{
        caller=>SOCKET
        },
    Merge_state=maps:merge(Call_info, STATE),
    case cstp:compute(CSTX, Merge_state) of
        {stop,Reason,NewState} ->
            {stop,Reason,NewState};
        {error, Reason} ->
            ft({error, [info,cstp]++Reason}),
            {noreply, STATE}
    end;
handle_info({ssl_error, SOCKET, _Reason}, STATE) ->
    TX = [
        _version={0,0,0},
        _service={0,0},     %system services
        _procedure={3,2},   %gate pRange, interface edge pPack
        _process={2,253}      %info pMod, ssl_error pRef
    ],
    {ok, CSTX} = cstp:compose(TX, STATE),
    Call_info = #{
        caller=>SOCKET
        },
    Merge_state=maps:merge(Call_info, STATE),
    case cstp:compute(CSTX, Merge_state) of
        {stop,Reason,NewState} ->
            {stop,Reason,NewState};
        {error, Reason} ->
            ft({error, [info,cstp]++Reason}),
            {noreply, STATE}
    end;
handle_info(INFO, STATE) ->
    log(["Info unexpected", INFO]),
    {noreply, STATE}.

%%  TERMINATE
terminate(_Reason, _STATE) ->
    crypto_man ! {exit, self()},
    ok.

%%  CODE_CHANGE
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.

%%%%%%%%%%%%
%%  BIFs  %%
%%%%%%%%%%%%
log(MSG) ->
    io:format("interface_edge| ~p~n", [MSG]).
%%% Fault tolerence
ft(FT)->
    log(["FT got ", FT]).
