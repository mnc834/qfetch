-module(qsrv).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([qrequest/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define (SRV_NAME, ?MODULE).
-define (URI, "https://api.robinhood.com/quotes/historicals").
-define (INTERVAL, "day").

-record(state, {
  request_map :: atom()| ets:tab()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SRV_NAME}, ?MODULE, [], []).

-spec qrequest(Symbol :: string()) -> {ok, term()} | {error, term()}.
%% @doc requests daily quotes for a Symbol
qrequest(Symbol) ->
  gen_server:call(?SRV_NAME, {qrequest, Symbol}).

%% gen_server.

init([]) ->
  {ok, #state{request_map = ets:new(request_map, [])}}.

handle_call({qrequest, Symbol}, From, #state{request_map = Request_map} = State) ->
  case httpc:request(get, {lists:append([?URI, "/", Symbol, "/?interval=", ?INTERVAL]), []}, [], [{sync, false}]) of
    {ok, Request_id} ->
      ets:insert(Request_map, {Request_id, From}),
      {noreply, State};
    {error, Reason} ->
      {reply, {error, {httpc_request_failed, Reason}}, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({http, {Request_id, Result}}, #state{request_map = Request_map} = State) ->
  case ets:lookup(Request_map, Request_id) of
    [{Request_id, From}] ->
      ets:delete(Request_map, Request_id),
      case  Result of
        {{_Version, 200, "OK"}, _Headers, Body} ->
          #{historicals := List} = jsx:decode(Body, [return_maps, {labels, atom}]),
          gen_server:reply(From, {ok, List});
        {{_Version, Status_code, Reason_phrase}, _Header, _Body} ->
          gen_server:reply(From, {error, {http_reply, Status_code, Reason_phrase}})
      end;
    [] ->
      %%log some error
      error
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
