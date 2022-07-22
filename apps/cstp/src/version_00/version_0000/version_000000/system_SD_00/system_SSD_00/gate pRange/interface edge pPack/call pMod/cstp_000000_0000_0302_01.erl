%%  Cessor Information Systems
%%  Cessor Service Transaction Protocol
%%
%%  Version Crown-Block index = 0
%%  Version Serial Domain = 0
%%  Version Serial number = 0
%%
%%  Service Domain = 0
%%  Service Sub-Domain = 0; System service
%%
%%  Procedure Range = 3;  Gate Packages Range
%%  Procedure Package = 2;  Interface Edge package
%%
%%  Process Module = 1;     Call module
-module(cstp_000000_0000_0302_01).

-export([compose/2]).
-export([compute/2]).

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%% Compose CSTX %%
%%%%%%%%%%%%%%%%%%
%%
%% Compose caller can be every processes id, PID
%% Data type guard
compose({_, TX}, _)when not is_binary(TX)->
    ft(_Issuer=compose, _Fault={error, [tx]});
%% Processor reference
compose({[{_module=1, _ref=0}, {STATUS}], TX}, STATE) ->
    processor_ref0({compose, STATUS, TX}, STATE);
compose(_Order, _STATE) ->
    ft(_Issuer=compose, _Fault={error, [order]}).
%%%%%%%%%%%%%%%%%%
%% Compute CSTX %%
%%%%%%%%%%%%%%%%%%
%%
%% Compute caller has to be interface_edge generic server.
%%
compute(TX, STATE)->
    case process_info(self()) of
        undefined ->
            ft(_Issuer=compute, _Fault={error, [info]});
        Info ->
            Dic = lists:keyfind(dictionary, 1, Info),
            case lists:keyfind('$ancestors', 1, Dic) of
                {'$ancestors',[interface_port, interface, core | _]} ->
                    compute(parse, TX, STATE);
                _ ->
                    ft(_Issuer=compute, _Fault={error, [permission]})
            end
    end.

%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse Processor Reference Information %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Proc_Ref 0 is Reserved for Parent STATUS Processor
%%
compute(parse, <<0, TX_/binary>>, STATE)->
    processor_ref0({compute, TX_}, STATE);

compute(parse, _TX, _STATE) ->
    ft(_Issuer=compute, _Fault={error, [parse]}).


%%%%%%%%%%%%%%%%%%%%%%%%
%%   Call Processors  %%
%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Parent STATUS Processor
%%  A set of tasks to.
%%  Request caller has to be interface_port generic server.
%%
processor_ref0({compose, STATUS, TX}, _STATE)
    when is_number(STATUS) andalso STATUS < 256->
        {ok, <<TX/binary, 1, STATUS>>};
processor_ref0({compute, <<STATUS>>}, STATE)->
    %%  STATUS 0:init, 1:boot, 2:ready, 3:challenge, 4:problem
    case whereis(interface_port) == maps:get(caller, STATE) of
        true ->
            TX = [
                    _version={0,0,0},
                    _service={0,0},     %system services
                    _procedure={3,2},   %Gate pRange, interface edge pPack
                    _process={0,1}      %state pMod, status pRef
                    , {STATUS}
                ],
            {ok, CSTX} = cstp:compose(TX, STATE),
            Process_refs = maps:get(proc_ref, STATE),
            Ref = make_ref(),
            gen_server:cast(self(), {Ref, CSTX}),
            STATE_proc_ref = maps:update(proc_ref, Process_refs++[Ref], STATE),
            {ok, _reply=ok, STATE_proc_ref};
        false ->
            ft(_Issuer=0, _Fault_={error, [permission]})
    end;
processor_ref0(_,_) ->
    ft(_Issuer=0, _Fault={error, [tx]}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%   Fault Tolerance  %%
%%%%%%%%%%%%%%%%%%%%%%%%

ft(Issuer, Fault)->
    Hader = 
        [
          {
            protocol,
            [
              0,0,0,      %version
              0,0,        %service
              3,2,        %procedure
              1           %module
            ]
          },
          {
            issuer,
            Issuer      %ft issuer  
          }
        ],
    case Fault of
        {error, Error} ->
            {
                error, 
                Hader ++ Error
            };
        {warn, Warning} ->
            {
                warn, 
                Hader ++ Warning
            }
    end.
