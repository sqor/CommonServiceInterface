#!/usr/bin/env escript
%% -*- erlang -*-
%%! -s inets start -name beamup@127.0.0.1

%% Usage:
%%   generate-config.sh --deps dep1 dep2 ... --build-env dev
%%
%% This script has two main functions. At first it makes possible to generate
%% sys.config files from template.sys.config file by replacing the variable
%% references from config.vars file, according to the build environment specified.
%%
%% It also does that in the specified dependencies and merge the sections from
%% the dependencies to the main sys.config.
%%
%% Example:
%%
%% deps/csi/config/sys.config
%% deps/ws/config/template.sys.config + config.vars
%% config/sys.config
%% config/override.vars (which has a special format like this)
%%
%%   {dev, [
%%      {"lager.log_root", "mystuff/log"}
%%   ]}.
%%   {stage, [
%%      %% This will be replaced
%%      {"lager.crash_log_count", 10},
%%      %% This will be removed
%%      "lager.error_logger_hwm"
%%   ]}.
%%
%% * get the csi config section from deps/csi/config/sys.config
%% * replace the variables in deps/ws/config/template.sys.config and generate
%%   a sys.config there
%% * get the ws secion from deps/ws/config/sys.config
%% * iterate over all sections in config/sys.config and
%%   * if there is no csi section, it copies the csi section of the 1st step
%%   * if there is such section, it keeps that
%%   ...
%%
%%   In that way we can collect the config of the dependencies but also we have
%%   possibility to override them one by one.
%% * Apply override.vars which contains rules for build envs. Like
%%   {"csi.timeout", 50} overrides the timeout key in csi config given that
%%     {csi, [
%%         ...
%%         {timeout, 10}
%%     ]}
%%   is the config. If there is no timeout key, it will add.
%%   We also can remove config value to specify only the key without value
%%   "csi.debug"

main(Args) ->
    Opts = parse_opts(Args),
    %% --deps are optional but if we don't have --build-env set, let us fail
    case lists:keyfind(env, 1, Opts) of
        false ->
            io:format("Usage: generate-config.sh --deps dep1 dep2 --build-env local|dev|...~n"),
            init:halt(1);
        _ ->
            ok
    end,

    {env, Env} = lists:keyfind(env, 1, Opts),
    Deps = get_deps(Opts),
    generate_deps(Deps, Env),
    generate_sys_config(".", Env),
    DepConfigs = lists:flatten([extract_dep_config(Dep) || Dep <- Deps]),
    Configs = apply_main_sys_config(DepConfigs),
    dump_configs("config/sys.config", Configs).

parse_opts(Opts) ->
    parse_opts(Opts, []).

parse_opts([], Result) ->
    Result;
parse_opts(["--deps" | Rest], Result) ->
    {Vals, Rest2} = take_values(Rest, []),
    parse_opts(Rest2, [{deps, Vals} | Result]);
parse_opts(["--build-env", Env | Rest], Result) ->
    parse_opts(Rest, [{env, Env} | Result]);
parse_opts(["--build-env"], Result) ->
    io:format("Default build-env=dev~n",[]),
    parse_opts([],[{env, "dev"} | Result]);
parse_opts(Opts, _Result) ->
    err("Cannot parse ~p~n", [Opts]).

take_values([], Vs) ->
    {Vs, []};
take_values([[$-, $- | _] | _] = Rest, Vs) ->
    {Vs, Rest};
take_values([V | Rest], Vs) ->
    take_values(Rest, Vs ++ [V]).

get_deps(Opts) ->
    case lists:keyfind(deps, 1, Opts) of
        false ->
            [];
        {deps, Deps} ->
            Deps
    end.

generate_sys_config(Dir, EnvName) ->
    %% Generate sys.config from template.sys.config depending on the env
    TemplateSysConfig = filename:join(Dir, "config/template.sys.config"),
    case filelib:is_file(TemplateSysConfig) of
        true ->
            process_template(Dir, EnvName);
        false ->
            ok
    end,
    %% Apply override vars if there is such a file, and there is sys.config
    SysConfig = filename:join(Dir, "config/sys.config"),
    Override = filename:join(Dir, "config/override.vars"),
    case filelib:is_file(SysConfig) andalso filelib:is_file(Override) of
        true ->
            apply_override(SysConfig, Override, EnvName);
        false ->
            ok
    end.

%% Generate sys.config from template.sys.config + config.vars
%% If template is missing, it left sys.config as it is (no templating there)
%% If config.vars is missing, it copies template to sys.config
process_template(Dir, EnvName) ->
    VarsFile = filename:join(Dir, "config/config.vars"),
    Template = filename:join(Dir, "config/template.sys.config"),
    SysConf = filename:join(Dir, "config/sys.config"),
    Vars = read_vars(VarsFile, EnvName),
    case file:read_file(Template) of
        {ok, File} ->
            case re:run(File, <<"\\${(.*?)}">>,
                        [{capture, all_but_first, binary}, global]) of
                nomatch ->
                    file:copy(Template, SysConf);
                {match, Vs} ->
                    %% Get the ${var} variable names
                    Vss = [binary_to_atom(V, utf8) || [V] <- Vs],
                    %% Replace them
                    Out = lists:foldl(
                            fun(Var, Acc) ->
                                case lists:keyfind(Var, 1, Vars) of
                                    {Var, Value} ->
                                        LV = case Value of
                                                 _ when is_integer(Value) ->
                                                     integer_to_list(Value);
                                                 _ when is_atom(Value) ->
                                                     atom_to_list(Value);
                                                 _ ->
                                                     "\"" ++ Value ++ "\""
                                             end,
                                        Re = "\\${" ++ atom_to_list(Var) ++ "}",
                                        re:replace(Acc, Re, LV, [global]);
                                    false ->
                                        err("No value for ~p~n", [Var])
                                end
                            end, File, Vss),
                    %% Write the output
                    file:write_file(SysConf, Out)
            end;
        {error, enoent} ->
            %% No template, leave sys.config as it is
            ok
    end.

apply_override(SysConfig, Override, EnvName) ->
    Env = list_to_atom(EnvName),
    {ok, [Sys]} = file:consult(SysConfig),
    {ok, Over} = file:consult(Override),
    case lists:keyfind(Env, 1, Over) of
        false ->
            ok;
        {_, Vars} ->
            Sys2 = lists:foldl(
                     fun(Var, Acc) ->
                             apply_path(Acc, Var)
                     end, Sys, Vars),
            dump_configs(SysConfig, Sys2)
    end.

atom_tokens(String) ->
    [list_to_atom(T) || T <- string:tokens(String, ".")].

%% Deep delete/replace a "key1.key2.key3" path in a term
apply_path(Term, {KeyPath, Value}) ->
    apply_path2(Term, atom_tokens(KeyPath), store, Value);
apply_path(Term, KeyPath) ->
    apply_path2(Term, atom_tokens(KeyPath), del, undefined).

%% Deep delete/replace a key list in a term
apply_path2(Term , [], _Op, _Value) ->
    Term;
apply_path2(Term, [Key], store, Value) ->
    lists:keystore(Key, 1, Term, {Key, Value});
apply_path2(Term, [Key], del, _Value) ->
    lists:keydelete(Key, 1, Term);
apply_path2(Term, [Key | Rest], Op, Value) ->
    SubTerm = case lists:keyfind(Key, 1, Term) of
                  false ->
                      [];
                  {Key, V} ->
                      V
              end,
    R = apply_path2(SubTerm, Rest, Op, Value),
    lists:keystore(Key, 1, Term, {Key, R}).

extract_dep_config(Dep) ->
    case file:consult(filename:join(["deps", Dep, "config/sys.config"])) of
        {ok, [Term]} ->
            Key = list_to_atom(Dep),
            lists:filter(
              fun({K, _}) when K =:= Key -> true;
                 (_)        -> false
              end, Term);
        _ ->
            []
    end.

apply_main_sys_config(DepConfigs) ->
    {ok, [Main]} = file:consult("config/sys.config"),
    %% Override the keys in DepConfigs with sys.config
    Result = lists:foldl(
               fun({Key, Value}, Acc) ->
                       case lists:keyfind(Key, 1, Main) of
                           false ->
                               %% New key, wasn't in the main sys.config
                               %% Let us add it to that
                               [{Key, Value} | Acc];
                           _ ->
                               %% Main sys.config overrides this key,
                               %% so do nothing here
                               Acc
                       end
               end, Main, DepConfigs),
    lists:keysort(1, Result).

dump_configs(Filename, Configs) ->
    {ok, Dev} = file:open(Filename, [write]),
    io:format(Dev, "~p.", [Configs]),
    file:close(Dev).

generate_deps(Deps, Env) ->
    [generate_sys_config("deps/" ++ Dep, Env) || Dep <- Deps].

read_vars(VarsFile, EnvName) ->
    case file:consult(VarsFile) of
        {ok, AllVars} ->
            Env = list_to_atom(EnvName),
            case lists:keyfind(Env, 1, AllVars) of
                {Env, Vars} ->
                    Vars;
                _ ->
                    io:format("Could not find vars for env:~p~n", [EnvName]),
                    []
            end;
        {error, enoent} ->
            [];
        ELSE ->
            io:format("heyhey:~p~n", [ELSE]),
            []
    end.

err(Msg, Args) ->
    io:format(Msg, Args),
    init:halt(1).

