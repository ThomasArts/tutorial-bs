%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2018, Thomas Arts
%%% @doc Super simple basics of a little blockchain server
%%%      This has none of the crypto's of blockchain, it just serves
%%%      as a simple example of testing a stateful data structure 
%%       server.
%%%
%%% @end
%%% Created : 27 May 2018 by Thomas Arts <thomas@ThomasComputer.lan>
%%%-------------------------------------------------------------------
-module(chain).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, spend/1, balance/1, 
         top/0, transactions/0, stop/0]).

%% API for crypto convenience
-export([generate_key/0, sign/2, verify_signature/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {blocks = [], transaction_pool = [], accounts = #{}}).
-record(block, {height = 0, prev_hash = 0, txs = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(ChainFile) ->
  {ok, Bin} = file:read_file(ChainFile),
  Chain = binary_to_term(Bin),
  gen_server:start_link({local, ?SERVER}, ?MODULE, Chain, []).

top() ->
  gen_server:call(?SERVER, top).

transactions() ->
  gen_server:call(?SERVER, transactions).

spend({signed, Tx, Signature}) ->
  case verify_signature(Tx, Signature, maps:get(sender, Tx)) of
    true ->
      gen_server:call(?SERVER, {spend, Tx});
    false ->
      {error, wrong_signature}
  end.

balance(Tx) ->
  gen_server:call(?SERVER, {balance, Tx}).

stop() ->
  gen_server:stop(?SERVER).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Read chain chain starts from block 1 and must fit on genesis
init(Chain) ->
  process_flag(trap_exit, true),
  {ok, [Genesis|_]} = file:consult("genesis"),  
  Accounts = add_transactions(Genesis, Chain, hash({1, Genesis})),
  spawn_link(fun() -> mine() end),
  {ok, #state{blocks = [#block{} | Chain], accounts = Accounts}}.

handle_call(transactions, _From, State) ->
  {reply, {ok, State#state.transaction_pool}, State};
handle_call(top, _From, State) ->
  TopBlock = lists:last(State#state.blocks),
  Reply = 
    #{height => TopBlock#block.height,
      prev_hash => TopBlock#block.prev_hash},
  {reply, {ok, Reply}, State};
handle_call({spend, #{sender := From, receiver := To, 
                      amount := A, fee := Fee}}, _From, State) ->
  case Fee > 0 of 
    true ->  {reply, ok, State#state{transaction_pool = 
                                      State#state.transaction_pool ++ [{spend, From, To, A, Fee}]}};
    false -> {reply, {error, fee_too_low}, State}
  end;
handle_call({balance, #{pubkey := PubKey}}, _From, State) ->
  Reply = 
    case maps:get(PubKey, State#state.accounts, undefined) of
      undefined -> {error, not_found};
      Account ->
        {ok, maps:get(balance, Account)}
    end,
  {reply, Reply, State}.

handle_cast(mined, State) ->
  {NewAccounts, LeftInPool} =
    add_txs(State#state.accounts, State#state.transaction_pool, []),
  Height = length(State#state.blocks),
  NewBlock = #block{prev_hash = hash({Height, State#state.accounts}),
                    height = Height,
                    txs = State#state.transaction_pool -- LeftInPool},
  {noreply, State#state{blocks = State#state.blocks ++ [NewBlock], 
                        accounts = NewAccounts, 
                        transaction_pool = LeftInPool}};
handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  %% io:format("Chain at termination ~p\n", [State#state.blocks]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_transactions(Accounts, [], _) ->
  Accounts;
add_transactions(Accounts, [#block{prev_hash = PrevHash, txs = Txs, height = H}| Chain ], PrevHash) ->
  {NewAccounts, []} = add_txs(Accounts, Txs, []),
  add_transactions(NewAccounts, Chain, hash({H, NewAccounts}));
add_transactions(_, Chain, _) ->
  error({cannot_validate, hd(Chain)}).

add_txs(Accounts, [], Unfit) ->
  {Accounts, lists:reverse(Unfit)};
add_txs(Accounts, [{spend, From, To, Amount, Fee} = Tx | Txs], Unfit) ->
  try
    FromAccount = maps:get(From, Accounts),
    true = (Fee >= 1 andalso maps:get(balance, FromAccount) >= Amount - Fee),
    ToAccount = maps:get(To, Accounts, #{balance => 0}),
    NewAccounts = 
      maps:merge(Accounts, #{From => maps:update(balance,  maps:get(balance, FromAccount) - Amount - Fee, FromAccount),
                             To => maps:update(balance,  maps:get(balance, ToAccount) + Amount, ToAccount)}),
    add_txs(NewAccounts, Txs, Unfit)
  catch
    _:_ ->
      add_txs(Accounts, Txs, [Tx | Unfit])
  end.


%%% =================================================================

mine() ->
  timer:sleep(500),
  gen_server:cast(?SERVER, mined), 
  mine().

%%% =================================================================

hash(X) ->
  erlang:phash2(X).

%% -> {Public, Private}
generate_key() ->
  crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)).

sign(Tx, PrivKey) ->
  Bin = serialize_to_binary(Tx),
  crypto:sign(ecdsa, sha256, Bin, [PrivKey, crypto:ec_curve(secp256k1)]).

serialize_to_binary(Map) ->
  term_to_binary(Map).   %% This is not a unique mapping!!

verify_signature(Tx, Signature, PubKey) ->
  Bin = serialize_to_binary(Tx),
  crypto:verify(ecdsa, sha256, Bin, Signature, [PubKey, crypto:ec_curve(secp256k1)]).
