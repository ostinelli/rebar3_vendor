-module(rebar3_prv_vendor_store).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, store).
-define(NAMESPACE, vendor).
-define(DEPS, [{default, install_deps}, {default, lock}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 vendor store"},
        {opts, []},
        {short_desc, "Makes a copy of dependencies to deps/ for vendoring."},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% init
    AllDeps = rebar_state:lock(State),
    DepsDir = rebar_dir:deps_dir(State),
    VendorDir = filename:join(rebar_dir:root_dir(State), "deps"),
    filelib:ensure_dir(filename:join([VendorDir, "dummy.beam"])),
    %% clean deps to ensure that no compile code is included
    clean_all_deps(State),
    %% zip all dependencies in the /deps directory
    rebar_api:info("Vendoring dependencies...", []),
    [begin
    %% get info
        Name = binary_to_list(rebar_app_info:name(Dep)),
        Vsn = get_vsn(Dep, State),
        %% prepare filename
        Filename = iolist_to_binary([Name, "-", Vsn, ".zip"]),
        Filepath = binary_to_list(filename:join([VendorDir, Filename])),
        %% purge other versions if they exist
        purge_other_versions(VendorDir, Filepath, Name),
        %% create zip if doesn't exist
        create_zip_if_not_exist(DepsDir, Filepath, Name)
    end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))],
    %% return
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec clean_all_deps(rebar_state:t()) -> ok.
clean_all_deps(State) ->
    %% temporary hack: add the 'all' option to be able to clean all dependencies
    {Args, Other} = rebar_state:command_parsed_args(State),
    State1 = rebar_state:command_parsed_args(State, {Args ++ [{all, true}], Other}),
    {ok, _} = rebar_prv_clean:do(State1),
    ok.

-spec get_vsn(rebar_app_info:t(), rebar_state:t()) -> binary() | string().
get_vsn(Dep, State) ->
    Dir = rebar_app_info:dir(Dep),
    Source = rebar_app_info:source(Dep),
    case rebar_fetch:lock_source(Dir, Source, State) of
        {git, _, {ref, Ref}} -> Ref;
        {pkg, _, Vsn0} -> Vsn0
    end.

-spec purge_other_versions(file:filename_all(), file:filename_all(), binary() | string()) -> list().
purge_other_versions(VendorDir, Filepath, Name) ->
    OtherFilepathPattern = filelib:wildcard(filename:join(VendorDir, string:concat(Name, "-*.zip"))),
    [begin
        rebar_api:info("   - ~s", [filename:basename(OtherFilepath, ".zip")]),
        ok = file:delete(OtherFilepath)
    end || OtherFilepath <- OtherFilepathPattern, OtherFilepath =/= Filepath].

create_zip_if_not_exist(DepsDir, Filepath, Name) ->
    case filelib:is_file(Filepath) of
        true ->
            rebar_api:debug("Skipping ~s: already vendored.", [filename:basename(Filepath, ".zip")]);
        false ->
            %% create zip   ===>
            rebar_api:info("   + ~s", [filename:basename(Filepath, ".zip")]),
            {ok, _} = zip:create(Filepath, [Name], [{cwd, DepsDir}])
    end.
