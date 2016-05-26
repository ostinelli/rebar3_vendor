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
    %% init
    DepsDir = rebar_dir:deps_dir(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    %% empty lib directory
    rebar_file_utils:rm_rf(DepsDir),
    filelib:ensure_dir(filename:join(DepsDir, "dummy.beam")),
    %% extract
    [begin
        Filename = filename:basename(Filepath, ".zip"),
        rebar_api:info("Extracting ~s", [Filename]),
        zip:extract(Filepath, [{cwd, DepsDir}])
    end || Filepath <- filelib:wildcard(filename:join(VendorDir, "*.zip"))],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).