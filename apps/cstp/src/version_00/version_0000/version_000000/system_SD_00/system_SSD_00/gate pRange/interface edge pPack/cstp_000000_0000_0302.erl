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
-module(cstp_000000_0000_0302).

-export([compose/2]).
-export([compute/2]).

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%% Compose CSTX
%%%     Data type guard
compose({_, TX}, _)when not is_binary(TX)->
    ft(_Issuer=compose, _Fault={error, [tx]});
%%%     Process Module
compose({Order=[{_module=0,_} | _rest], TX}, STATE) ->
    cstp_000000_0000_0302_00:compose({Order, <<TX/binary, 0>>}, STATE);
compose({Order=[{_module=1,_} | _rest], TX}, STATE) ->
    cstp_000000_0000_0302_01:compose({Order, <<TX/binary, 1>>}, STATE);
compose(_Order, _STATE) ->
    ft(_Issuer=compose, _Fault={error, [order]}).
%%% Compute CSTX
%%% 
%%% Parse Process Module Information
%%% 
%%%     Process Module
%%%         Proc_Module 0 is Reserved for State Module
compute(<<Module, TX_/binary>>, STATE)
  when Module == 0->
    cstp_000000_0000_0302_00:compute(TX_, STATE);
%%%         Proc_Module 1 is Reserved for Call Module
compute(<<Module, TX_/binary>>, STATE)
  when Module == 1->
    cstp_000000_0000_0302_01:compute(TX_, STATE);

compute(_TX, _STATE) ->
    ft(_Issuer=compute,_Fault={error, [tx]}).

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
              3,2         %procedure  
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