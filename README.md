Rebar3 Vendoring Plugin
=====

Plugin for storing vendored depdenencies and applying the vendored deps to your local project.

Add the plugin to your rebar config:

    {plugins, [rebar3_vendor]}.


To store the fetched dependencies under `./deps/` for committing:

    $ rebar3 vendor store

To take the vendored dependencies from `./deps/` and place them under the build directory in the appropriate place:

    $ rebar3 vendor apply
