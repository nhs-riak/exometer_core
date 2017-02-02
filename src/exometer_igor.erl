%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014-2017 Basho Technologies, Inc.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_igor).
-export([parse_transform/2]).

% Starting with OTP-19 dialyzer complains about the parameters to
% igor:parse_transform/2.
% I've looked through the specs for the relevant functions in the OTP sources
% and it all looks correct here, but the OTP sources have conflicting spec
% attributes and doc comments, which may be where dialyzer's getting munged up.
% For now, I'm telling dialyzer to STFU about it, but it would be good to
% resolve it more comprehensively.
-dialyzer(no_return).
-dialyzer(no_fail_call).
-dialyzer(no_unused).

-spec parse_transform(Forms :: erl_syntax:forms(), Opts :: list())
            -> [erl_syntax:syntaxTree()].
parse_transform(Forms, Opts) ->
    IgorOpts = lists:append([Os || {attribute,_,compile,{igor,Os}} <- Forms]),
    io:fwrite("IgorOpts = ~p~nOpts = ~p", [IgorOpts, Opts]),
    Includes = [I || {i,I} <- Opts],
    NewOpts = [{igor, IgorOpts
        ++ [{includes, Includes}, {preprocess, true}]} | Opts],
    NewForms = igor:parse_transform(Forms, NewOpts),
    fix_for_r16b03(NewForms).

%% erl_syntax:revert/1 is horribly broken in R16B03. This transform
%% corrects
fix_for_r16b03({'fun',L1,{function,{atom,_,F},{integer,_,A}}}) ->
    {'fun',L1,{function,F,A}};
fix_for_r16b03({'fun',L1,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}) ->
    {'fun',L1,{function,M,F,A}};
fix_for_r16b03(F) when is_tuple(F) ->
    list_to_tuple([fix_for_r16b03(X) || X <- tuple_to_list(F)]);
fix_for_r16b03([H|T]) ->
    [fix_for_r16b03(H) | fix_for_r16b03(T)];
fix_for_r16b03(F) ->
    F.

