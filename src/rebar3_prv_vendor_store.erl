-module(rebar3_prv_vendor_store).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, store).
-define(NAMESPACE, vendor).
-define(DEPS, [{default, lock}]).

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
            {example, "rebar3 vendor store"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Makes a copy of dependencies to deps/ for vendoring."},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Vendoring dependencies...", []),
    AllDeps = rebar_state:lock(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    [begin
         Dir = rebar_app_info:dir(Dep),
         filelib:ensure_dir(filename:join([VendorDir, "dummy.beam"])),
         rebar_file_utils:cp_r([Dir], VendorDir)
     end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
