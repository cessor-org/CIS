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
%% @end
%%%-------------------------------------------------------------------

-module(cstp_000000_0000_0000_00).

-export([compose/2]).
-export([compute/2]).

-import(cstp_ft, [ft/2]).
%%%%%%%%%%%%%%%
%%   EXPORT  %%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%
%%   Compose CSTX  %%
%%%%%%%%%%%%%%%%%%%%%
%%
%% compose({Order, TX}, STATE)-> Result
%%  Order = list of tuples, [Process | Rest]
%%          Process = tuple of numbers, {PM, Processor_ref}
%%                    PM = number, >=0 && <256, process module
%%                    Processor_ref = number, >=0 && <256, process ID
%%          Rest = list of tuples, [ Parameters ]
%%  TX = binary, recursive composed TX
%%  STATE = map, state of caller has to be compatible with order resources
%%  Result =
%%      {ok,TX_}
%%          TX_ = binary, recursive composed TX
%%                      
%%      {error,  Reason}
%%          Generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = compose
%%              Fault =
%%                {error, [order]}, order format is unavailable
%% INFO:
%%      Compose a cessor cervice transaction, CSTX from an order list
%%      This module composes orders with Version {CBI=0, VSD=0, VSN=0}
%%      This module composes orders with Service {SD=0, SSD=0}
%%      This module composes orders with Procedure {PR=0, PP=0}
%%      This module composes orders with Process Module PM=0
%%      Processor reference pRef=0; is reserved for INIT Processor
%%      Processor reference pRef=1; is reserved for BOOT Processor
%%      Processor reference pRef=2; is reserved for STATUS Processor
%%      Compose caller can be every processes id, PID
%%
compose({[{_module=0, _ref=0}], TX}, STATE) ->
    processor_ref0({compose, TX}, STATE);
compose({[{_module=0, _ref=1}], TX}, STATE) ->
    processor_ref1({compose, TX}, STATE);
compose({[{_module=0, _ref=2}], TX}, STATE) ->
    processor_ref2({compose, TX}, STATE);

compose(Order, _STATE) ->
    ft(_Issuer={?MODULE, compose}, _Fault={error, [order, Order]}).
%%%%%%%%%%%%%%%%%%%%%
%%   Compute CSTX  %%
%%%%%%%%%%%%%%%%%%%%%
%%
%% compute(TX, STATE)-> Result
%%  TX = binary, << Process, Rest >>
%%          Process = binary, << Processor_ref >>
%%                    Processor_ref = number, >=0 && <256, process ID
%%          Rest = binary, << Parameters >>
%%  STATE = map, state of caller has to be compatible with the resources needed to compute TX
%%  Result =
%%      {ok,Call_back}
%%          Call_back = term
%%                      
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = compute
%%              Fault =
%%                {error, [permission]}, Caller has no permission
%% INFO:
%%      Compute a cessor cervice transaction, CSTX
%%      Processor reference pRef=0 has to be called by cessor_unit application
%%      Processor reference pRef=k has to be called by core generic server
%%
compute(TX= <<0, _/binary>>, STATE)->
    case whereis(cessor_unit) == self() of
        true ->
            compute(parse, TX, STATE);
        false ->
            ft(_Issuer={?MODULE, compute}, _Fault={error, [permission]})
    end;
compute(TX, STATE)->
    case whereis(core) == self() of
        true ->
            compute(parse, TX, STATE);
        false ->
            ft(_Issuer={?MODULE, compute}, _Fault={error, [permission]})
    end.

%%%%%%%%%%%%%%%%%
%%   INTERNAL  %%
%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse Processor Reference Information %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% compute({parse, TX}, STATE)-> Result
%%  TX = binary, << Process, Rest >>
%%          Process = binary, << Processor_ref >>
%%                    Processor_ref = number, >=0 && <256, process ID
%%          Rest = binary, << Parameters >>
%%  STATE = map, state of caller has to be compatible with the resources needed to compute TX
%%  Result =
%%      Result of processor_ref*k/2
%%                      
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = compute
%%              Fault =
%%                {error, [parse]}, Tx format is not available
%% INFO:
%%      Compute a cessor cervice transaction, CSTX
%%      Parse Processor References to compute
%%      Processor reference pRef=0; is reserved for INIT Processor
%%      Processor reference pRef=1; is reserved for BOOT Processor
%%      Processor reference pRef=2; is reserved for STATUS Processor
%%
compute(parse, <<0, TX_/binary>>, STATE)->
    processor_ref0({compute, TX_}, STATE);

compute(parse, <<1, TX_/binary>>, STATE)->
    processor_ref1({compute, TX_}, STATE);
compute(parse, <<2, TX_/binary>>, STATE)->
    processor_ref2({compute, TX_}, STATE);
compute(parse, _TX, _STATE) ->
    ft(_Issuer={?MODULE, compute}, _Fault={error, [parse]}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%   State Processors  %%
%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%
%%   INIT Processor  %%
%%%%%%%%%%%%%%%%%%%%%%%
%%
%% processor_ref0({compose, TX}, _)-> Result
%%  TX = binary, recursive composed TX
%%  
%%  Result =
%%      {ok,TX_}
%%          TX_ = binary, recursive composed TX
%% INFO:
%%      Compose CSTX for processor_ref0
%%      A set of tasks to init the state of core generic server
%%
processor_ref0({compose, TX}, _STATE)->
    {ok, <<TX/binary, 0>>};
%%
%% processor_ref0({compute, TX}, _)-> Result
%%  TX = binary, <<  >>
%%  
%%  Result =
%%      {ok,Core}
%%          Core = pid, PID of core generic server
%%      {error, Reason :: term()}
%% INFO:
%%      Compute CSTX
%%      Start and link new core generic server
%%
processor_ref0({compute, <<>>}, STATE)->
    core:start_link(STATE);
%%
%% processor_ref0(_,_)-> Result
%%  Result =
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = processor_ref0
%%              Fault =
%%                {error, [tx]}, Tx format is not available
%% INFO:
%%      Computation is failed because of unavailable arguments
%%
processor_ref0(_,_) ->
    ft(_Issuer={?MODULE, processor_ref0}, _Fault={error, [tx]}).

%%%%%%%%%%%%%%%%%%%%%%%
%%   BOOT Processor  %%
%%%%%%%%%%%%%%%%%%%%%%%
%%
%% processor_ref1({compose, TX}, _)-> Result
%%  TX = binary, recursive composed TX
%%  
%%  Result =
%%      {ok,TX_}
%%          TX_ = binary, recursive composed TX
%% INFO:
%%      Compose CSTX for processor_ref1
%%      A set of tasks to setup system services of core generic server
%%
processor_ref1({compose, TX}, STATE)->
    PIN = maps:get(pin, STATE),
    {ok, <<TX/binary, 1, PIN/binary>>};
%%
%% processor_ref1({compute, TX}, _)-> Result
%%  TX = binary, << PIN >>
%%  
%%  Result =
%%      {ok,New_state}
%%          New_state = map, the updated STATE
%%      {error, Reason :: term()}
%% INFO:
%%      Compute CSTX
%%      Run components
%%      Start and link new crypto_man generic server
%%
processor_ref1({compute, PIN}, STATE)->
    TX_crypto = [
        _version={0,0,0},
        _service={0,0},     %system services
        _procedure={1,0},   %crypto pRange, crypto_man pPack
        _process={0,0}      %state pMod, INIT pRef
    ],
    {ok, CSTX_crypto} = cstp:compose(TX_crypto, #{pin=>PIN}),
    io:format("CSTX_crypto: ~p~n", [CSTX_crypto]),
    Ref_crypto = make_ref(),
    gen_server:cast(self(), {Ref_crypto, CSTX_crypto}),

    TX_store = [
        {0,0,0},    %version
        {0,0},      %service:system services
        {2,0},      %procedure:store pRange, storage pPack
        {0,0}       %process:state pMod, INIT pRef
    ],
    {ok, CSTX_store} = cstp:compose(TX_store, #{}),
    io:format("CSTX_store: ~p~n", [CSTX_store]),
    Ref_store = make_ref(),
    gen_server:cast(self(), {Ref_store, CSTX_store}),

    TX_gate = [
        {0,0,0},    %version
        {0,0},      %service:system services
        {3,0},      %procedure:gate pRange, interface pPack
        {0,0}       %process:state pMod, INIT pRef
    ],
    {ok, CSTX_gate} = cstp:compose(TX_gate, #{}),
    io:format("CSTX_gate: ~p~n", [CSTX_gate]),
    Ref_gate = make_ref(),
    gen_server:cast(self(), {Ref_gate, CSTX_gate}),


    TX_status = [
        {0,0,0},    %version
        {0,0},      %service:system services
        {0,0},      %procedure:unit pRange, core pPack
        {0,2}       %process:state pMod, STATUS pRef
    ],
    {ok, CSTX_status} = cstp:compose(TX_status, #{}),
    io:format("CSTX_status: ~p~n", [CSTX_status]),
    Ref_status = make_ref(),
    gen_server:cast(self(), {Ref_status, CSTX_status}),

    New_ref = [Ref_crypto, Ref_store, Ref_gate, Ref_status],
    Proc_ref = maps:get(proc_ref, STATE) ++ New_ref,
    New_state = maps:update(proc_ref, Proc_ref, STATE),
    {ok, New_state};
%%
%% processor_ref1(_,_)-> Result
%%  Result =
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = processor_ref1
%%              Fault =
%%                {error, [tx]}, Tx format is not available
%% INFO:
%%      Computation is failed because of unavailable arguments
%%
processor_ref1(_,_) ->
    ft(_Issuer={?MODULE, processor_ref1}, _Fault={error, [tx]}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%   STATUS Processor  %%
%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% processor_ref2({compose, TX}, _)-> Result
%%  TX = binary, recursive composed TX
%%  
%%  Result =
%%      {ok,TX_}
%%          TX_ = binary, recursive composed TX
%% INFO:
%%      Compose CSTX for processor_ref2
%%      A recursive composed TX
%%
processor_ref2({compose, TX}, _STATE)->
    {ok, <<TX/binary, 2>>};
%%
%% processor_ref2({compute, TX}, _)-> Result
%%  TX = binary, << PIN >>
%%  
%%  Result =
%%      {ok,New_state}
%%          New_state = map, the updated STATE
%%      {error, Reason :: term()}
%% INFO:
%%      Compose CSTX for processor_ref2
%%      A set of tasks to report system serviceses state and interpret new status
%%      If need upgrade the service status and propogate it
%%
processor_ref2({compute, <<>>}, STATE)->
    Report = processor_ref2({status, report}, STATE),
    processor_ref2({interpret, Report}, STATE);


processor_ref2({status, report}, STATE)->
    Services = maps:get(service, STATE),
    %%  STATUS unset, init, ready, challenge, problem
    Statuses =
        [
            {unset, []},
            {init, []},
            {boot, []},
            {ready, []}
            %%  Other statuses
        ],
    Fun =
        fun
            ({Name, _pid, unset}, Report) ->    %%  UNSET
                {unset, UNSET} = lists:keyfind(unset, 1, Report),
                lists:keyreplace(
                    unset
                    ,1
                    ,Report
                    ,{boot,UNSET++[Name]}
                );
            ({Name, _pid, init}, Report) ->    %% INIT
                {init, INIT} = lists:keyfind(init, 1, Report),
                lists:keyreplace(
                    init
                    ,1
                    ,Report
                    ,{init,INIT++[Name]}
                );
            ({Name, _pid, boot}, Report) ->    %% BOOT
                {boot, BOOT} = lists:keyfind(boot, 1, Report),
                lists:keyreplace(
                    boot
                    ,1
                    ,Report
                    ,{boot,BOOT++[Name]}
                );
            ({Name, _pid, ready}, Report) ->    %%  READY
                {ready, READY} = lists:keyfind(ready, 1, Report),
                lists:keyreplace(
                    ready
                    ,1
                    ,Report
                    ,{ready,READY++[Name]}
                )
        end,
    lists:foldl(Fun, Statuses, Services);


processor_ref2({interpret, Report}, STATE)->
    %   List of services with status of UNSET
    %{unset, UNSET} = lists:keyfind(unset, 1, Report),
    %   List of services with status of INIT
    {init, INIT} = lists:keyfind(init, 1, Report),
    %   List of services with status of BOOT
    {boot, BOOT} = lists:keyfind(boot, 1, Report),
    %   List of services with status of READY
    {ready, READY} = lists:keyfind(ready, 1, Report),
    Services_len = length(maps:get(service, STATE)),

    case maps:get(status, STATE) of
        unset
            when Services_len == length(INIT)->
            STATUS = <<"INIT">>,
            Broad_list = [crypto, store, gate],
            processor_ref2({broadcast, STATUS, Broad_list}, STATE);
        init
            when Services_len == length(BOOT)->
                STATUS = <<"BOOT">>,
                Broad_list = [],
                processor_ref2({broadcast, STATUS, Broad_list}, STATE);
        boot
            when Services_len == length(READY)->
                STATUS = <<"READY">>,
                Broad_list = [],
                processor_ref2({broadcast, STATUS, Broad_list}, STATE);
        _ ->
            {ok, STATE}
    end;

processor_ref2({broadcast, STATUS, Broad_list}, STATE)->
    Broadcast =
        fun
            (Name, ok) ->
                {Procedure, PID} =
                    case Name of
                        core ->     {{0,0}, whereis(core)};
                        crypto ->   {{1,0}, whereis(crypto_man)};
                        store ->    {{2,0}, whereis(storage)};
                        gate ->     {{3,0}, whereis(interface)}
                    end,
                TX = [
                    {0,0,0},        %version
                    {0,0},          %service:system services
                    Procedure,      %procedure: {pRange, pPack}
                    {1,0}           %process:call pMod, System STATUS pRef
                    , STATUS        %status:STATUS
                ],
                {ok, CSTX} = cstp:compose(TX, STATE),
                try
                    gen_server:call(PID, CSTX)
                catch
                    exit:_ ->
                        {error, [call, Name]}
                end;
            (_,ERR) ->
                ERR
        end,
    case lists:foldl(Broadcast, ok, Broad_list) of
        ok->
            STATE_status = maps:update(status, STATUS, STATE),
            {ok, STATE_status};
        ERR->
            ft(_Issuer={?MODULE, processor_ref2},
                _Fault={error, [broadcast]++ERR})
    end;
%%
%% processor_ref2(_,_)-> Result
%%  Result =
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = processor_ref2
%%              Fault =
%%                {error, [tx]}, Tx format is not available
%% INFO:
%%      Computation is failed because of unavailable arguments
%%
processor_ref2(_,_) ->
    ft(_Issuer={?MODULE, processor_ref2}, _Fault={error, [tx]}).