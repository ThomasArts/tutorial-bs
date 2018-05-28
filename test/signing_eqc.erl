%%% @author Thomas Arts <thomas@ThomasComputer.lan>
%%% @copyright (C) 2018, Thomas Arts
%%% @doc QuickCheck signing of messages
%%%      Use: rebar3 as test shell --apps ""
%%%      to get an Erlang shell for playing with these properties
%%% @end
%%% Created : 27 May 2018 by Thomas Arts <thomas@ThomasComputer.lan>

-module(signing_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

%% generator
term() ->
  oneof([list(nat()), int(), bool(), map(real(), binary())]).


%% positive testing
prop_sign() ->
  {PubKey, PrivKey} = chain:generate_key(),
  ?FORALL(Term, term(),
     ?WHENFAIL(eqc:format("failing for Key: ~p\n", [PubKey]),
     collect(Term,
          begin
            Signature = chain:sign(Term, PrivKey),
            chain:verify_signature(Term, Signature, PubKey)
          end))).

%% negative testing
%% do it yourself
