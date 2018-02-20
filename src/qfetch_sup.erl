-module(qfetch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Processes = [
    {
      qsrv,
      {qsrv, start_link, []},
      permanent,
      1,
      worker,
      [qsrv]
    }
  ],

  {ok, {{one_for_one, 1, 5}, Processes}}.
