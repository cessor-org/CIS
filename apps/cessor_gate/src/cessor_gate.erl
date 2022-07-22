-module(cessor_gate).

-export([gate_app_run/0]).
-export([set_callback/1]).

-on_load(init/0).

-define(APPNAME, cessor_gate).
-define(LIBNAME, libcessor_gate).

%%====================================================================
%% API functions
%%====================================================================

gate_app_run()->
    not_loaded(?LINE).
set_callback(_) ->
    not_loaded(?LINE).


%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    ENV=application:get_all_env(),
    io:format("GATE ENV: ~p~n", [ENV]),
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true -> filename:join(["..", priv, ?LIBNAME]);
                _ -> filename:join([priv, ?LIBNAME])
            end;
        Dir -> filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).