%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mar 2018 10:02 PM
%%%-------------------------------------------------------------------
-module(qfetch).
-author("MNC834").

%% API
-export([start/0]).

%% @doc Start the application. Mainly useful for using `-s qfetch' as a command
%% line switch to the VM to make lager start on boot.
start() -> start(qfetch).

start(App) ->
  start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
  ok = start(Dep),
  start(App);
start_ok(App, {error, Reason}) ->
  erlang:error({app_start_failed, App, Reason}).
