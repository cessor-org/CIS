%%%-------------------------------------------------------------------
%% @doc
%%  https://cessor.org
%%  Cessor Information Systems, CIS
%%  CIS License v0.1.0
%%  Cessor Service Transaction Protocol
%%  Version Crown-Block index = 0
%% @end
%%%-------------------------------------------------------------------

-module(cstp_00).

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
%%  Order = list of tuples, [Version | Rest]
%%          Version = tuple of numbers, {CBI, VSD, VSN}
%%                    CBI = number, >=0 && <256, the Crown-Block Index
%%                    VSD = number, >=0 && <256, version serial domain
%%                    VSN = number, >=0 && <256, version serial number
%%          Rest = list of tuples, [ Service, Procedure, Process, Parameters ]
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
%%      Currently, this module composes orders with CBI = 0 on first Crown tree
%%      Currently, this module composes orders with VSD = 0
%%
compose({Order=[_version={0,0,_} | _rest], TX}, STATE) ->
    cstp_0000:compose({Order, <<TX/binary, 0>>}, STATE);
compose(_Order, _STATE) ->
    ft(_Issuer={?MODULE, compose}, _Fault={error, [order]}).

%%%%%%%%%%%%%%%%%%%%%
%%   Compute CSTX  %%
%%%%%%%%%%%%%%%%%%%%%
%%
%% compute(TX, STATE)-> Result
%%  TX = binary, << Version, Rest >>
%%          Version = binary, << VSD, VSN >>
%%                    VSD = number, >=0 && <256, version serial domain
%%                    VSN = number, >=0 && <256, version serial number
%%          Rest = binary, << Service, Procedure, Process, Parameters >>
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
%%                {error, [tx]}, Tx format is not available
%% INFO:
%%      Compute a cessor cervice transaction, CSTX
%%      Currently, this module composes orders with VSD = 0
%%
compute(<<VersionSerialDomain, TX_/binary>>, STATE)
  when VersionSerialDomain == 0 ->
    cstp_0000:compute(TX_, STATE);

compute(_TX, _STATE) ->
    ft(_Issuer={?MODULE, compute},_Fault={error, [tx]}).

%%%%%%%%%%%%%%%%%
%%   INTERNAL  %%
%%%%%%%%%%%%%%%%%