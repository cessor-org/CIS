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
%%  Procedure Range = 3;        Gate Procedure Packages Range
%%  Procedure Package = 3;      Interface_remote package
%%  Process Module = 1;         Call Process Module
%% @end
%%%-------------------------------------------------------------------
-module(cstp_000000_0000_0303_01).

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
%%      This module composes orders with Procedure {PR=3, PP=3}
%%      This module composes orders with Process Module PM=1
%%      Processor reference pRef=0; is reserved for System STATUS Processor
%%      Compose caller can be every processes id, PID
%%
%% Processor reference
compose({[{_module=1, _ref=0}, STATUS], TX}, STATE)->
        processor_ref0({compose, STATUS, TX}, STATE);

compose(_Order, _STATE) ->
    ft(_Issuer={?MODULE, compose}, _Fault={error, [order]}).
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
%%                {error, [operation]}, No operation
%% INFO:
%%       No operation
%%
compute(_TX, _STATE)->
    ft(_Issuer={?MODULE, compute}, _Fault={error, [operation]}).

%%%%%%%%%%%%%%%%%
%%   INTERNAL  %%
%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%   Call Processors  %%
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   System STATUS Processor  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% processor_ref0({compose, TX}, _)-> Result
%%  TX = binary, recursive composed TX
%%  
%%  Result =
%%      {ok,TX_}
%%          TX_ = binary, recursive composed TX
%% INFO:
%%      Compose CSTX for processor_ref0
%%      A set of tasks to integrate state with System status
%% 
processor_ref0({compose, STATUS, TX}, _STATE)->
    {ok, <<TX/binary, 0, STATUS/binary>>};
%%
%% processor_ref0(_, _)-> Result
%%  Result =
%%      {error,  Reason}
%%          Reason = generated by Fault Tolerance
%%            ft(Issuer, Fault) 
%%              Issuer = processor_ref0
%%              Fault =
%%                {error, [tx]}, TX format is unavailable
%% INFO:
%%      Computation is failed because of unavailable arguments
%%
processor_ref0(_,_) ->
    ft(_Issuer={?MODULE, processor_ref0}, _Fault={error, [tx]}).