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
%%  Procedure Package = 1;      Interface_port package
%%  Process Module = 0;         State Process Module
%%  Process Module = 1;         Call Process Module
%%%%%%%%%%%% Interface_port Generic Server %%%%%%%%%%%%
%% @end
%%%-------------------------------------------------------------------
-module(interface_port).

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
    TX = [
        _version={0,0,0},
        _service={0,0},     %system services
        _procedure={3,1},   %Gate pRange, interface port pPack
        _process={0,1}      %state pMod, boot pRef
    ],
    {ok, CSTX} = cstp:compose(TX, #{}),
    Ref = make_ref(),
    gen_server:cast(self(), {Ref, CSTX}),
    % Service status
    STATUS = init,  % unset, init, boot, ready
    % Listen Socket
    Listen_Socket = [],
    % Edge List
    Edge_List = [], %%  {edge_pid, Status}
    % Reference list of asynchronous requests
    Reference_list = [Ref],
    % Service state
    STATE =
        #{
            status => STATUS
            , port => Listen_Socket
            , edges=>Edge_List
            , proc_ref=> Reference_list
        },
    case register(interface_port, self()) of
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
            ,3,1    %gate pRange, interface port pPack
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
handle_info({{client, Client}, connect}, STATE) ->
    Clients = maps:get(clients, STATE),
    New_Clients = lists:keyreplace(Client, 1, Clients, {Client, connect}),
    New_STATE = maps:update(clients, New_Clients, STATE),
    gen_server:cast(self(), new_interface),
    {noreply, New_STATE};
handle_info({{client, Client}, active}, STATE) ->
    Clients = maps:get(clients, STATE),
    New_Clients = lists:keyreplace(Client, 1, Clients, {Client, active}),
    New_STATE = maps:update(clients, New_Clients, STATE),
    gen_server:cast(self(), new_interface),
    {noreply, New_STATE};
handle_info({{client, Client}, {error, _Reason}}, STATE) ->
    Clients = maps:get(clients, STATE),
    New_Clients = lists:keydelete(Client, 1, Clients),
    New_STATE = maps:update(clients, New_Clients, STATE),
    gen_server:cast(self(), new_interface),
    %%  ft ! Reason,
    {noreply, New_STATE};
handle_info(INFO, STATE) ->
    log(["Info unexpected", INFO]),
    {noreply, STATE}.

%%  TERMINATE
terminate(_Reason, STATE) ->
    LS = maps:get(ls, STATE),
    ssl:close(LS),
    ok.

%%  CODE_CHANGE
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.

%%  BIFs
log(MSG) ->
    io:format("interface_port| ~p~n", [MSG]).
%%% Fault tolerence
ft(FT)->
    log(["FT got ", FT]).