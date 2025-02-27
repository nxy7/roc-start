module
    {
        env_var!,
        is_file!,
        read_utf8!,
        write_utf8!,
    } -> [Config, load_dotfile!, create_default_dotfile!, default_config, save_to_dotfile!, save_config!]

import parse.Parse as P
import Theme exposing [Theme]
LogLevel : [Silent, Quiet, Verbose]

Config : { verbosity : LogLevel, theme : Theme, platform : Str }

config_to_str : Config -> Str
config_to_str = |config|
    verbosity =
        when config.verbosity is
            Verbose -> "verbose"
            Quiet -> "quiet"
            Silent -> "silent"
    """
    verbosity: ${verbosity}
    theme: ${config.theme.name}
    platform: ${config.platform}
    """

load_dotfile! : {} => Result Config [HomeVarNotSet, NoDotFileFound, InvalidDotFile, FileReadError]
load_dotfile! = |{}|
    home = env_var!("HOME") ? |_| HomeVarNotSet
    file_path = "${home}/.rocstartconfig"
    if file_exists!(file_path) then
        file_contents = read_utf8!(file_path) ? |_| FileReadError
        parse_dotfile(file_contents) |> Result.map_err(|_| InvalidDotFile)
    else
        Err(NoDotFileFound)

file_exists! = |path| is_file!(path) |> Result.with_default(Bool.false)

parse_dotfile : Str -> Result Config [InvalidDotFile]
parse_dotfile = |str|
    lines = Str.split_on(str, "\n")
    verbosity =
        when
            lines |> List.keep_oks(parse_verbosity)
        is
            [level, ..] -> level
            _ -> default_config.verbosity
    theme =
        when
            lines |> List.keep_oks(parse_theme)
        is
            [colors, ..] -> colors
            _ -> default_config.theme
    platform =
        when
            lines |> List.keep_oks(parse_platform)
        is
            [pf, ..] -> pf
            _ -> default_config.platform
    Ok({ verbosity, theme, platform })

parse_theme = |str|
    themes = [Theme.roc.name, Theme.warn_only.name, Theme.no_color.name] |> List.map(|s| P.string(s))
    pattern = P.string("theme:") |> P.rhs(P.maybe(P.whitespace)) |> P.rhs(P.one_of(themes))
    parser =
        pattern
        |> P.map(
            |s|
                if s == Theme.warn_only.name then
                    Ok(Theme.warn_only)
                else if s == Theme.no_color.name then
                    Ok(Theme.no_color)
                else if s == Theme.roc.name then
                    Ok(Theme.roc)
                else
                    Err(InvalidTheme),
        )
    parser(str) |> P.finalize |> Result.map_err(|_| InvalidTheme)

parse_verbosity = |str|
    verbosity_levels = ["silent", "quiet", "verbose"] |> List.map(|s| P.string(s))
    pattern = P.string("verbosity:") |> P.rhs(P.maybe(P.whitespace)) |> P.rhs(P.one_of(verbosity_levels))
    parser =
        pattern
        |> P.map(
            |s|
                when s is
                    "silent" -> Ok(Silent)
                    "quiet" -> Ok(Quiet)
                    "verbose" -> Ok(Verbose)
                    _ -> Err(InvalidLogLevel),
        )
    parser(str) |> P.finalize |> Result.map_err(|_| InvalidLogLevel)

parse_platform = |str|
    parser = P.string("platform:") |> P.rhs(P.maybe(P.whitespace)) |> P.rhs(platform_string)
    parser(str) |> P.finalize |> Result.map_err(|_| InvalidPlatform)

platform_string = P.one_or_more(platform_chars) |> P.map(|chars| Str.from_utf8_lossy(chars) |> Ok)

platform_chars = P.char |> P.filter(|c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (List.contains(['-', '_', '/'], c)))

create_default_dotfile! : {} => Result Config [HomeVarNotSet, FileWriteError]
create_default_dotfile! = |{}|
    save_config!(default_config)?
    Ok(default_config)

save_config! : Config => Result {} [HomeVarNotSet, FileWriteError]
save_config! = |config|
    home = env_var!("HOME") ? |_| HomeVarNotSet
    file_path = "${home}/.rocstartconfig"
    contents = config_to_str(config)
    write_utf8!(contents, file_path) |> Result.map_err(|_| FileWriteError)

default_config = { verbosity: Verbose, theme: Theme.roc, platform: "basic-cli" }

save_to_dotfile! : { key : Str, value : Str } => Result {} [HomeVarNotSet, FileWriteError, FileReadError]
save_to_dotfile! = |{ key, value }|
    home = env_var!("HOME") ? |_| HomeVarNotSet
    file_path = "${home}/.rocstartconfig"
    if file_exists!(file_path) then
        file_contents = read_utf8!(file_path) ? |_| FileReadError
        Str.split_on(file_contents, "\n")
        |> List.map(
            |line|
                if Str.starts_with(line, key) then
                    "${key}: ${value}"
                else
                    line,
        )
        |> |lines|
            if List.contains(lines, "${key}: ${value}") then
                lines
            else
                List.append(lines, "${key}: ${value}\n")
        |> Str.join_with("\n")
        |> write_utf8!(file_path)
        |> Result.map_err(|_| FileWriteError)
    else
        "${key}: ${value}\n"
        |> write_utf8!(file_path)
        |> Result.map_err(|_| FileWriteError)
