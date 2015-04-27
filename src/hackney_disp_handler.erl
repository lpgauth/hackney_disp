%%% -*- erlang -*-
%%%
%%% This file is part of hackney_disp released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012, Frederic Trottier-Hebert under the BSD license
%%%
%%% @doc Dispcount worker implementation for TCP socket handling
%%% @end
%%%

-module(hackney_disp_handler).
-include_lib("hackney/include/hackney.hrl").

-behaviour(dispcount).
-export([
    checkin/2,
    checkout/2,
    code_change/3,
    dead/1,
    handle_info/2,
    init/1,
    terminate/2
]).

-record(state, {
    resource = {error, undefined},
    given = false,
    init_arg,
    transport
}).

%% dispcount callbacks
checkin(Socket, #state {
        resource = {ok, Socket},
        given = true,
        transport= Transport
    } = State) ->

    Transport:setopts(Socket, [{active, once}]),
    {ok, State#state {given = false}};
checkin(_Socket, State) ->
    {ignore, State}.

checkout(_From, State = #state {given = true}) ->
    {error, busy, State};
checkout(From, State = #state {
        resource = {ok, Socket},
        transport = Transport
    }) ->

    Transport:setopts(Socket, [{active, false}]),
    case Transport:controlling_process(Socket, From) of
        ok ->
            {ok, {self(), Socket}, State#state {given = true}};
        % {error, badarg} ->
        %     Transport:setopts(Socket, [{active, once}]),
        %     {error, caller_is_dead, State};
        {error, Reason} ->
            lager:warning("hackney checkout error: ~p~n", [Reason]),
            case reconnect(State) of
                {ok, NewState} -> checkout(From, NewState);
                Return -> Return
            end
    end;
checkout(From,  #state {
        resource = {error, _Reason}
    } = State) ->

    case reconnect(State) of
        {ok, NewState} -> checkout(From, NewState);
        Return -> Return
    end;
checkout(From, State) ->
    {stop, {invalid_call, From, State}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

dead(State) ->
    case reconnect(State#state {given = false}) of
        {ok, NewState} -> {ok, NewState};
        {error, _Reason, NewState} -> {ok, NewState}
    end.

handle_info(_Msg, State) ->
    {ok, State}.

init(Init) ->
    {ok, #state {init_arg = Init}}.

terminate(_Reason, _State) ->
    ok.

%% private
lookup(Key, List) ->
    lookup(Key, List, undefined).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

reconnect(State = #state {init_arg = InitArg}) ->
    {Host, Port, Transport, Opts} = InitArg,
    ConnectOpts0 = lookup(connect_options, Opts, []),
    ConnectOpts1 = case hackney_util:is_ipv6(Host) of
        true -> [inet6 | ConnectOpts0];
        false -> ConnectOpts0
    end,

    ConnectOpts = case Transport of
        hackney_ssl_transport ->
            case lookup(ssl_options, Opts) of
                undefined ->
                    case lookup(insecure, Opts) of
                        true ->
                            ConnectOpts1 ++ [
                                {verify, verify_none},
                                {reuse_sessions, true}
                            ];
                        _ -> ConnectOpts1
                    end;
                SslOpts -> ConnectOpts1 ++ SslOpts
            end;
        _ -> ConnectOpts1
    end,

    ConnectTimeout = lookup(connect_timeout, Opts, 8000),
    case Transport:connect(Host, Port, ConnectOpts, ConnectTimeout) of
        {ok, Socket} ->
            {ok, State#state {
                resource = {ok, Socket},
                transport = Transport
            }};
        {error, Reason} ->
            Key = {Transport, Host, Port},
            lager:warning("hackney reconnect error ~p: ~p~n", [Key, Reason]),
            {error, Reason, State#state {
                resource = {error, Reason},
                transport=Transport
            }}
    end.
