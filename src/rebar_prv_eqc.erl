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
    EqcOpts = resolve_eqc_opts(State),

    {EqcFun, TestQuantity} = numtests_or_testing_time(EqcOpts),

    ok = rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER),

    ProjectApps = project_apps(State),
    Properties = properties(app_modules(app_names(ProjectApps), [])),
    Result = make_result([{Property,
                           eqc:quickcheck(eqc:EqcFun(TestQuantity,
                                                     Mod:Property()))}
                          || {Mod, Property} <- Properties]),
    case handle_results(Result) of
        {error, Reason} ->
            ?PRV_ERROR(Reason);
        ok ->
            {ok, State}
    end.

numtests_or_testing_time(Opts) ->
    case lists:keyfind(numtests, 1, Opts) of
        false ->
            lists:keyfind(testing_time, 1, Opts);
        NumTests ->
            NumTests
    end.

-spec properties(Modules) -> Properties when
      Modules :: [atom()],
      Properties :: [{atom(), atom()}].
%% Scan the exports for each module for properties and return a list of
%% `{Module, Property}' pairs
properties(Modules) ->
    lists:flatten(
      [lists:filtermap(property_filter_fun(M),
                       M:module_info(exports)) || M <- Modules]).

-type property_filter_fun() :: fun(({atom(), non_neg_integer()}) ->
                                          boolean() | {true, {atom(), atom()}}).
-spec property_filter_fun(atom()) -> property_filter_fun().
property_filter_fun(Module) ->
    fun({Function, 0}) ->
        %% Properties must be 0-arity
        BinFun = atom_to_binary(Function, latin1),
        case BinFun of
            <<"prop_", _/binary>> ->
                {true, {Module, Function}};
            _ ->
                false
        end;
       (_) ->
        false
    end.

-spec app_names([rebar_app_info:t()]) -> [atom()].
app_names(Apps) ->
    [binary_to_atom(rebar_app_info:name(A), unicode) || A <- Apps].

make_result(Results) ->
    Fails = lists:filtermap(fun({_, true}) -> false;
                               ({Prop, false}) -> {true, Prop} end,
                            Results),
    case Fails of
        [] ->
            ok;
        _ ->
            {error, Fails}
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
    {Opts, _} = rebar_state:command_parsed_args(State),
    EqcOpts = rebar_state:get(State, eqc_opts, []),
    TestingTime = proplists:get_value(testing_time, Opts),
    NumTests = proplists:get_value(numtests, Opts),
    set_test_quantifier(NumTests, TestingTime, EqcOpts).

set_test_quantifier(undefined, undefined, Opts) ->
    NumTestsPresent = lists:keymember(numtests, 1, Opts),
    TestTimePresent = lists:keymember(testing_time, 1, Opts),
    if
        NumTestsPresent andalso TestTimePresent ->
            lists:keydelete(testing_time, 1, Opts);
        NumTestsPresent ->
            Opts;
        TestTimePresent ->
            maybe_keep_testing_time(Opts);
        true ->
            lists:keystore(numtests, 1, Opts, {numtests, 100})
    end;
set_test_quantifier(undefined, Time, Opts) ->
    maybe_add_testing_time(Time, Opts);
set_test_quantifier(NumTests, undefined, Opts) ->
    lists:keystore(numtests, 1, Opts, {numtests, NumTests}).

%% Only add the testing_time option if the function is actually
%% available in the EQC module. This is only the case for licensed
%% versions of EQC.
maybe_add_testing_time(Time, Opts) ->
    case testing_time_available() of
        true ->
            lists:keystore(testing_time, 1, Opts, {testing_time, Time});
        false ->
            maybe_add_default_numtests(Opts)
    end.

maybe_add_default_numtests(Opts) ->
    case lists:keymember(numtests, 1, Opts) of
        true ->
            Opts;
        false ->
            lists:keystore(numtests,
                           1,
                           lists:keydelete(testing_time, 1, Opts),
                           {numtests, 100})
    end.

%% Only keep the testing_time option if the function is actually
%% available in the EQC module. This is only the case for licensed
%% versions of EQC.
maybe_keep_testing_time(Opts) ->
    case testing_time_available() of
        true ->
            Opts;
        false ->
            lists:keystore(numtests,
                           1,
                           lists:keydelete(testing_time, 1, Opts),
                           {numtests, 100})
    end.

testing_time_available() ->
    lists:keymember(testing_time, 1, eqc:module_info(exports)).

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, FailedProps}) ->
    {error, {properties_failed, lists:flatten(FailedProps)}}.

eqc_opts(_State) ->
    [
     {numtests, $n, "numtests", integer, help(numtests)},
     {testing_time, $t, "testtime", integer, help(testing_time)}
    ].

help(numtests) -> "The number of times to execute each property";
help(testing_time) -> "Time (secs) to spend executing each property. "
                         "The testtime and numtests options are "
                         "mutually exclusive. If both are specified "
                         "numtests is used. Use of this option requires "
                         "the full version of EQC.".
