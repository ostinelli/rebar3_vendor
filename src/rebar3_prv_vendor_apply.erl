-module(rebar3_prv_vendor_apply).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, apply).
-define(NAMESPACE, vendor).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 vendor apply"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, ""},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Applying vendored dependencies...", []),
    DepsDir = rebar_dir:deps_dir(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    filelib:ensure_dir(filename:join(DepsDir, "dummy.beam")),
    rebar_file_utils:cp_r(filelib:wildcard(filename:join(VendorDir, "*")), DepsDir),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
