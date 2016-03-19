-module(rebar3_vendor).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_prv_vendor_store:init(State),
    {ok, State2} = rebar3_prv_vendor_apply:init(State1),
    {ok, State2}.
