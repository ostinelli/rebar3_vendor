[![Hex pm](https://img.shields.io/hexpm/v/rebar3_vendor.svg)](https://hex.pm/packages/rebar3_vendor)

# Rebar3 Vendoring Plugin

Plugin for storing vendored dependencies and applying the vendored deps to your local project.

## Install
Add the plugin to your rebar config, which should be at `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [rebar3_vendor]}.
```

## Usage
To store the fetched dependencies under `./deps/` for committing:

```
$ rebar3 vendor store
```

To take the vendored dependencies from `./deps/` and place them under the build directory in the appropriate place:

```
$ rebar3 vendor apply
```
