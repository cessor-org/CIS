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
%%  Process Module = 2;     Info module
-module(cstp_000000_0000_0302_02).

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
%%
compose({_, TX}, _)when not is_binary(TX)->
    ft(_Issuer=compose, _Fault={error, [tx]});
%% Processor reference
compose({[{_module=2, _ref=0}, {STATUS}], TX}, STATE) ->
    processor_ref0({compose, STATUS, TX}, STATE);
compose({[{_module=2, _ref=1}, {Message}], TX}, STATE) ->
    processor_ref1({compose, Message, TX}, STATE);

compose({[{_module=2, _ref=253}], TX}, STATE) ->
    processor_ref253({compose, TX}, STATE);
compose({[{_module=2, _ref=254}], TX}, STATE) ->
    processor_ref254({compose, TX}, STATE);
compose(_Order, _STATE) ->
    ft(_Issuer=compose, _Fault={error, [order]}).
%%%%%%%%%%%%%%%%%%
%% Compute CSTX %%
%%%%%%%%%%%%%%%%%%
%%
%% Compute caller has to be interface_edge generic server.
%% Request caller has to be SOCKET port.
%%
compute(TX, STATE)->
    case process_info(self()) of
        undefined ->
            ft(_Issuer=compute, _Fault={error, [info]});
        Info ->
            Dic = lists:keyfind(dictionary, 1, Info),
            case lists:keyfind('$ancestors', 1, Dic) of
                {'$ancestors',[interface_port, interface, core | _]} ->
                    Port = maps:get(port, STATE),
                    case maps:get(caller, STATE) == Port of
                        true->
                            compute(parse, TX, STATE);
                        false ->
                            ft(_Issuer=compute, _Fault={error, [port]})
                    end;
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
%% Proc_Ref 0 is Reserved for Key State Processor
%% Proc_Ref 1 is Reserved for Send Message Processor
%%
%% Proc_Ref 253 is Reserved for SSL error event Processor
%% Proc_Ref 254 is Reserved for SSL closed event Processor
%% Proc_Ref 255 is Reserved for module extension
%%
compute(parse, <<0, TX_/binary>>, STATE)->
    processor_ref0({compute, TX_}, STATE);
compute(parse, <<1, TX_/binary>>, STATE)->
    processor_ref1({compute, TX_}, STATE);
compute(parse, <<253, TX_/binary>>, STATE)->
    processor_ref253({compute, TX_}, STATE);
compute(parse, <<254, TX_/binary>>, STATE)->
    processor_ref254({compute, TX_}, STATE);
compute(parse, _TX, _STATE) ->
    ft(_Issuer=compute, _Fault={error, [parse]}).


%%%%%%%%%%%%%%%%%%%%%%%%
%%   Info Processors  %%
%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Key State Report Processor
%%  A set of tasks to report key state information like key status, public key list.
%%
processor_ref0({compose, STATUS, TX}, _STATE)
    when is_number(STATUS) andalso STATUS < 256->
        {ok, <<TX/binary, 1, STATUS>>};
processor_ref0({compute, <<>>}, STATE)->
    TX = [
            _version={0,0,0},
            _service={0,0},     %system services
            _procedure={1,0},   %crypto pRange, crypto_man pPack
            _process={1,2}      %call pMod, key state report pRef
        ],
    {ok, CSTX} = cstp:compose(TX, STATE),
    try
        case gen_server:call(crypto_man, CSTX) of
            {ok, Report} ->
                %%  Using same module processor to response the request
                processor_ref1({compute, Report}, STATE);
            {error, Reason} ->
                ft(_Issuer=0, _Fault={error, [call, crypto_man]++Reason})
        end
    catch
        exit:_ ->
            ft(_Issuer_=0, _Fault_={error, [call, crypto_man]})
    end;
processor_ref0(_,_) ->
    ft(_Issuer=0, _Fault={error, [tx]}).
%%
%%  Send Message Processor
%%  A set of tasks to send messages to current tls socket.
%%
processor_ref1({compose, Message, TX}, _STATE)
    when is_binary(Message)->
        {ok, <<TX/binary, 1, Message>>};
processor_ref1({compute, Message}, STATE)->
    Port = maps:get(port, STATE),
    try
        Port ! Message,
        {ok , STATE}
    catch
        exit:_ ->
            ft(_Issuer_=1, _Fault_={error, [send, port]})
    end;
processor_ref1(_,_) ->
    ft(_Issuer=1, _Fault={error, [tx]}).
%%
%%  SSL error event Processor
%%  A set of tasks to .
%%  
processor_ref253({compose, TX}, _STATE)->
        {ok, <<TX/binary, 253>>};
processor_ref253({compute, <<>>}, STATE)->
    processor_ref253({call, interface_port, {status, ssl_error}}, STATE);
processor_ref253({call, interface_port, {status, ssl_error}}, STATE)->
    TX_status = [
            _version={0,0,0},
            _service={0,0},     %system services
            _procedure={3,1},   %gate pRange, interface port pPack
            _process={1,1}      %call pMod, children status pRef
            , _status={253}     %ssl_error status
        ],
    {ok, CSTX_core} = cstp:compose(TX_status, STATE),
    try
        ok=gen_server:call(interface_port, CSTX_core),
        Reason = normal,
        {stop,Reason,STATE}
    catch
        exit:_ ->
            ft(_Issuer=253, _Fault={error, [call, interface_port]})
    end;
processor_ref253(_,_) ->
    ft(_Issuer=253, _Fault={error, [tx]}).
%%
%%  SSL closed event Processor
%%  A set of tasks to .
%%  
processor_ref254({compose, TX}, _STATE)->
        {ok, <<TX/binary, 254>>};
processor_ref254({compute, <<>>}, STATE)->
    processor_ref254({call, interface_port, {status, ssl_closed}}, STATE);
processor_ref254({call, interface_port, {status, ssl_closed}}, STATE)->
    TX_status = [
            _version={0,0,0},
            _service={0,0},     %system services
            _procedure={3,1},   %gate pRange, interface port pPack
            _process={1,1}      %call pMod, children status pRef
            , _status={254}     %ssl_closed status
        ],
    {ok, CSTX_core} = cstp:compose(TX_status, STATE),
    try
        ok=gen_server:call(interface_port, CSTX_core),
        Reason = normal,
        {stop,Reason,STATE}
    catch
        exit:_ ->
            ft(_Issuer=254, _Fault={error, [call, interface_port]})
    end;
processor_ref254(_,_) ->
    ft(_Issuer=254, _Fault={error, [tx]}).
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
              2           %module
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
