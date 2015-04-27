%%% -*- erlang -*-
%%%
%%% This file is part of hackney_disp released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012, Frederic Trottier-Hebert under the BSD license
%%%
%%% @doc Dispatcher for hackney using dispcount. Starts pools and
%%% handles checkin/checkout.  The user shouldn't have to touch this
%%% module.  @end
%%%

-module(hackney_disp).
-include_lib("hackney/include/hackney.hrl").

-behaviour(hackney_pool_handler).
-export([
    start/0,
    checkout/4,
    checkin/2
]).

-export([
    notify/2
]).

-define(TABLE, ?MODULE).

%% hackney_pool_handler callbacks
start() ->
    hackney_util:require([dispcount]).

checkout(Host, Port, Transport, #client {options=Opts}) ->
    Info = find_disp({Host, Port, Transport}, Opts),
    case dispcount:checkout(Info) of
        {ok, CheckinReference, {Owner, Socket}} ->
            {ok, {Info, CheckinReference, Owner, Transport}, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

checkin({Info, Ref, Owner, Transport}, Socket) ->
    try Transport:controlling_process(Socket, Owner) of
        ok -> dispcount:checkin(Info, Ref, Socket);
        {error, _Reason} -> dispcount:checkin(Info, Ref, dead)
    catch error:badarg ->
        dispcount:checkin(Info, Ref, dead)
    end.

%% public
notify(_Pool, _Msg) ->
    ok.

%% private
find_disp(Key, Args) ->
    case ets:lookup(hackney_pool, Key) of
        [] -> start_disp(Key, Args);
        [{_,Info}] -> Info
    end.

pool_name(Host, Port, hackney_ssl_transport) ->
    list_to_atom("dispcount_" ++ Host ++ integer_to_list(Port) ++ "_ssl");
pool_name(Host, Port, hackney_tcp_transport) ->
    list_to_atom("dispcount_" ++ Host ++ integer_to_list(Port)).

start_disp(Key = {Host, Port, Transport}, Opts0) ->
    {ok, Type} = application:get_env(hackney, restart),
    {ok, Timeout} = application:get_env(hackney, shutdown),
    {ok, MaxR} = application:get_env(hackney, maxr),
    {ok, MaxT} = application:get_env(hackney, maxt),
    Opts = hackney_util:maybe_apply_defaults([max_connections], Opts0),
    MaxConn = proplists:get_value(max_connections, Opts, 25),
    AtomKey = pool_name(Host, Port, Transport),

    Res = dispcount:start_dispatch(AtomKey,
        {hackney_disp_handler, {Host, Port, Transport, Opts}}, [
            {dispatch_mechanism, round_robin},
            {maxr, MaxR},
            {maxt, MaxT},
            {resources, MaxConn},
            {restart, Type},
            {shutdown, Timeout}
    ]),

    case Res of
        ok ->
            {ok, Info} = dispcount:dispatcher_info(AtomKey),
            ets:insert(hackney_pool, {Key, Info}),
            Info;
        already_started ->
            find_disp(Key, Opts)
    end.
