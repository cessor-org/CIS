%%%-------------------------------------------------------------------
%% @doc
%%  https://cessor.org
%%  Cessor Information Systems, CIS
%%  CIS License v0.1.0
%%  Cessor Service Transaction Protocol Fault Tolerance
%% @end
%%%-------------------------------------------------------------------

-module(cstp_ft).

-export([ft/2]).
%%%%%%%%%%%%%%%
%%   EXPORT  %%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%
%%   Fault Tolerance  %%
%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ft({MODULE, Issuer}, Fault)-> Result
%%  MODULE = atom, isuuer module name
%%  Issuer = atom, issuer name
%%  Fault = tuple, {F_type, Err_list}
%%          F_type = atom, type of fault
%%                    error
%%                    warn
%%          Err_list = list of term, error list
%%  Result =
%%      {error,  [Header | Err_list]}
%%      {warn,  [Header | Err_list]}
%%          Header = list of term, [MODULE, Issuer]
%%                MODULE = {module, M}
%%                        M = Module name of FT issuer
%%                Issuer = atom, issuer name
%%          Err_list = list of term, error list
%%            
%% INFO:
%%      Generate a fault-tolerance object
%%
ft({MODULE, Issuer}, Fault)->
    Hader = 
        [
          {module, MODULE},
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
    end;
ft({MODULE, Issuer}, Fault) ->
    {error, [ft, {MODULE, Issuer}, Fault]}.