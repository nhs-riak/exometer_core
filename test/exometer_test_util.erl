%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2017 Basho Technologies, Inc.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------

-module(exometer_test_util).

-export([
    app_lib_dir/0,
    majority/2,
    majority/3,
    setup_test_log/0
]).

-define(MAJORITY_COUNT_ENV, "CT_MAJORITY_COUNT").
-define(DEFAULT_MAJORITY_COUNT, 9).

majority(F, Cfg) ->
    case os:getenv(?MAJORITY_COUNT_ENV) of
        false ->
            majority(?DEFAULT_MAJORITY_COUNT, F, Cfg);
        Count ->
            case catch erlang:list_to_integer(Count) of
                C when is_integer(C) ->
                    majority(C, F, Cfg);
                _ ->
                    ct:pal("Invalid value for '~s' given", [?MAJORITY_COUNT_ENV]),
                    majority(?DEFAULT_MAJORITY_COUNT, F, Cfg)
            end
    end.

%% Run test N times. Success if a majority of the tests succeed.
%% Cleanup between runs done by calling F({cleanup, Config})
%% Returns 'ok' or {error, Info}.
%%
majority(N, F, Cfg) ->
    majority(N, F, Cfg, []).

majority(0, _, _, Hist) ->
    Failed = length([1 || {caught,_,_} <- Hist]),
    LogMsg = lists:flatten(io_lib:format("majority: Failed = ~p, Hist = ~p", [Failed, Hist])),
    ct:pal(LogMsg),
    case {Failed, length(Hist)} of
        {Lf, L} when Lf >= L div 2 ->
            ct:fail({error, {too_many_failures, Hist}});
        _ ->
            {comment, LogMsg}
    end;
majority(N, F, Cfg, Hist) when N > 0 ->
    Res = try F(Cfg)
          catch
              C:R ->
                  {caught, C, R}
          after
              F({cleanup, Cfg})
          end,
    majority(N-1, F, Cfg, [Res|Hist]).

%% Return the runtime app directory, differs in Rebar2 vs 3.
app_lib_dir() ->
    Key = {?MODULE, test_dir},
    case erlang:get(Key) of
        undefined ->
            % Use this in case code:which/1 would return cover_compiled and
            % we'd have to revert to it anyway - the code is obviously already
            % loaded.
            {_, _, Beam} = code:get_object_code(?MODULE),
            Dir = filename:dirname(filename:dirname(Beam)),
            _ = erlang:put(Key, Dir),
            Dir;
        Val ->
            Val
    end.

%% Set up logging with reasonable test outputs.
setup_test_log() ->
    LogDir = filename:join(app_lib_dir(), "log"),
    ConLog = filename:join(LogDir, "console.log"),
    ErrLog = filename:join(LogDir, "error.log"),
    CrashLog = filename:join(LogDir, "crash.log"),
    filelib:ensure_dir(ConLog),
    application:load(sasl),
    application:set_env(sasl, errlog_type, error),
    application:load(lager),
    application:set_env(lager, crash_log, CrashLog),
    application:set_env(lager, handlers, [
        {lager_console_backend, warn},
        {lager_file_backend, [{file, ErrLog}, {level, warn}]},
        {lager_file_backend, [{file, ConLog}, {level, debug}]} ]).
