%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_eqc).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("eqc/include/eqc.hrl").

-define(PROVIDER, eqc).
-define(DEPS, [compile]).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {bare, false},
                                 {example, "rebar3 eqc"},
                                 {short_desc, "Run EQC properties."},
                                 {desc, ""},
                                 {opts, eqc_opts(State)},
                                 {profiles, [eqc]}]),
    State1 = rebar_state:add_provider(State, Provider),
    State2 = rebar_state:add_to_profile(State1, eqc, test_state(State1)),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    eqc:start(),
    ?INFO("Running EQC tests...", []),
    case prepare_tests(State) of
        {ok, Tests} ->
            do_tests(State, Tests);
        Error ->
            Error
    end.

-spec format_error(any()) -> iolist().
format_error(unknown_error) ->
    io_lib:format("Error running tests", []);
format_error({properties_failed, FailedProps}) ->
    io_lib:format("The following properties failed: ~p", [FailedProps]).

%% ===================================================================
%% Internal functions
%% ===================================================================

do_tests(State, _Tests) ->
    _EqcOpts = resolve_eqc_opts(State),

    ok = rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER),

    ProjectApps = project_apps(State),
    TestMods = app_modules(app_names(ProjectApps), []),
    Result = make_result([eqc:module(Test) || Test <- TestMods]),
    case handle_results(Result) of
        {error, Reason} ->
            ?PRV_ERROR(Reason);
        ok ->
            {ok, State}
    end.

-spec app_names([rebar_app_info:t()]) -> [atom()].
app_names(Apps) ->
    [binary_to_atom(rebar_app_info:name(A), unicode) || A <- Apps].

make_result(Results) ->
    case lists:all(fun([]) -> true; (_) -> false end, Results) of
        true ->
            ok;
        false ->
            {error, Results}
    end.

test_state(State) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    first_files(State) ++ [{erl_opts, TestOpts}].

safe_define_test_macro(Opts) ->
    %% defining a compile macro twice results in an exception so
    %% make sure both 'TEST' and 'EQC' are only defined once
    Opts1 = case test_defined(Opts) of
                true -> Opts;
                false -> [{d, 'TEST'}] ++ Opts
            end,
    case eqc_defined(Opts1) of
        true -> Opts1;
        false -> [{d, 'EQC'}] ++ Opts1
    end.

test_defined(Opts) ->
    directive_defined(Opts, 'TEST').

eqc_defined(Opts) ->
    directive_defined(Opts, 'EQC').

directive_defined([{d, Directive}|_], Directive) ->
    true;
directive_defined([{d, Directive, true}|_], Directive) ->
    true;
directive_defined([_|Rest], _Directive) ->
    directive_defined(Rest, _Directive);
directive_defined([], _) ->
    false.

first_files(State) ->
    EUnitFirst = rebar_state:get(State, eunit_first_files, []),
    [{erl_first_files, EUnitFirst}].

prepare_tests(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    resolve_apps(State, RawOpts).

resolve_apps(State, RawOpts) ->
    compile_tests(State, project_apps(State), all, RawOpts).

compile_tests(State, TestApps, Suites, RawOpts) ->
    F = fun(AppInfo) ->
        AppDir = rebar_app_info:dir(AppInfo),
        S = case rebar_app_info:state(AppInfo) of
            undefined ->
                C = rebar_config:consult(AppDir),
                rebar_state:new(State, C, AppDir);
            AppState ->
                AppState
        end,
        ok = rebar_erlc_compiler:compile(replace_src_dirs(S),
                                         ec_cnv:to_list(rebar_app_info:out_dir(AppInfo)))
    end,
    lists:foreach(F, TestApps),
    ok = maybe_cover_compile(State, RawOpts),
    {ok, test_set(TestApps, Suites)}.

maybe_cover_compile(State, Opts) ->
    State1 = case proplists:get_value(cover, Opts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

project_apps(State) ->
    filter_checkouts(rebar_state:project_apps(State)).

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    case rebar_app_info:is_checkout(App) of
        true  -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

app_modules([], Acc) -> Acc;
app_modules([App|Rest], Acc) ->
    Unload = case application:load(App) of
        ok                           -> true;
        {error, {already_loaded, _}} -> false
    end,
    NewAcc = case application:get_key(App, modules) of
        {ok, Modules} -> Modules ++ Acc;
        undefined     -> Acc
    end,
    case Unload of
        true  ->
            application:unload(App),
            app_modules(Rest, NewAcc);
        false ->
            app_modules(Rest, NewAcc)
    end.

replace_src_dirs(State) ->
    %% replace any `src_dirs` with just the `test` dir
    ErlOpts = rebar_state:get(State, erl_opts, []),
    StrippedOpts = lists:keydelete(src_dirs, 1, ErlOpts),
    rebar_state:set(State, erl_opts, [{src_dirs, ["test"]}|StrippedOpts]).

test_set(Apps, all) -> set_apps(Apps, []).

set_apps([], Acc) -> lists:reverse(Acc);
set_apps([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    set_apps(Rest, [{application, AppName}|Acc]).

resolve_eqc_opts(State) ->
    %% TODO: Check for command line properties
    {_Opts, _} = rebar_state:command_parsed_args(State),
    rebar_state:get(State, eqc_opts, []).

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, FailedProps}) ->
    {error, {properties_failed, lists:flatten(FailedProps)}}.

eqc_opts(_State) ->
    [
     %% {props, $p, "props", string, help(props)},
     %% {cover, $c, "cover", boolean, help(cover)},
     %% {eunit, $e, "eunit", boolean, help(eunit)}
    ].

%% TODO
%% help(mods)  -> "Run the properties from the specified modules";
%% help(props) -> "List of eqc properties to run.";
%% help(cover) -> "Generate cover data";
%% help(eunit) -> "Set EQC compiler directive and run eunit tests."
