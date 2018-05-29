%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2018, Thomas Arts
%%% @doc Model for blockchain testing
%%%
%%% @end
%%% Created : 27 May 2018 by Thomas Arts <thomas@ThomasComputer.lan>

-module(transactions_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile([export_all, nowarn_export_all]).

%% -- State and state functions ----------------------------------------------
-record(state, {accounts = []}).

-record(account, {pubkey, privkey, balance}).

initial_state() ->
  #state{}.

%% -- Generators -------------------------------------------------------------

account_gen(NatGen) ->
    ?LET(Balance, NatGen,
         begin
           {PubKey, PrivKey} = chain:generate_key(),
           #account{ pubkey = PubKey, balance = Balance, privkey = PrivKey }
         end).

%% -- Operations -------------------------------------------------------------

%% --- Operation: create_account ---
create_account_pre(_S) ->
  true.

create_account_args(_S) ->
  [patron(), account_gen(elements([0, 100, 200])), choose(1,10)].

create_account_pre(_S, [_Patron, _Account, _Fee]) ->
  true.

create_account(Patron, Account, Fee) ->
  Tx = #{sender => Patron#account.pubkey,
         receiver => Account#account.pubkey,
         amount => Account#account.balance,
         fee => Fee},
  chain:spend({signed, Tx, chain:sign(Tx, Patron#account.privkey)}).

create_account_next(S, _Value, [_Patron, Account, _]) ->
  S#state{accounts = S#state.accounts ++ [Account] }.

create_account_post(_S, [_Patron, _Account, _], Res) ->
  eq(Res, ok).

create_account_features(S, [_Patron, _Account, _Fee], _Res) ->
  [ {create, length(S#state.accounts) + 1} ].



%% --- Operation: spend ---
spend_pre(_S) ->
  true.

spend_args(_S) ->
  [].

spend_pre(_S, _Args) ->
  true.

spend() ->
  ok.

spend_next(S, _Value, _Args) ->
  S.

spend_post(_S, _Args, _Res) ->
  true.


%% --- ... more operations

%% -- Property ---------------------------------------------------------------

patron() ->
  #account{pubkey = <<4,79,47,147,159,65,213,178,205,195,196,237,58,156,121,
                      35,205,196,136,202,31,185,211,248,12,0,33,228,194,176,
                      244,61,56,28,105,144,30,55,113,47,200,113,38,12,241,134,
                      80,5,183,111,217,75,141,107,142,125,211,97,54,72,254,
                      135,117,51,236>>,
           privkey = <<146,131,12,25,46,46,110,68,92,178,53,46,46,46,229,243,
                       56,92,23,18,209,28,174,9,196,201,15,218,248,163,110,147>>,
           balance = 100000000}.


prop_transactions() ->
  ?SETUP(fun() -> 
             eqc_cover:start(), 
             fun() -> 
                 Data = eqc_cover:stop(),
                 eqc_cover:write_html(Data, [{out_dir, "cover"}])
             end
         end,
  eqc:dont_print_counterexample(
  ?FORALL(Cmds, commands(?MODULE),
  begin
    {ok, _Pid} = chain:start_link(),

    {H, S, Res} = run_commands(Cmds),

    {ok, TransactionPool} = chain:transactions(),
    FinalBalances = 
      lists:foldl(fun(A, Acc) ->
                      case chain:balance(#{pubkey => A#account.pubkey}) of
                        {ok, B} when B == A#account.balance -> Acc;
                        Other -> {A, wrong_balance, Other} 
                      end
                  end, [], S#state.accounts), 

    chain:stop(),

    check_command_names(Cmds,
      measure(length, commands_length(Cmds),
      aggregate(call_features(H),
        pretty_commands(?MODULE, Cmds, {H, S, Res},
                        conjunction([{result, Res == ok},
                                     {balances, equals(FinalBalances, [])},
                                     {transactions, equals(TransactionPool, [])}
                                    ])))))
  end))).
