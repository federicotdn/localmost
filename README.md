# localmost

A flexible and deterministic Claude Code `PreToolUse` tool based on [ShellCheck](https://www.shellcheck.net/).

## How it Works

Localmost uses the [PreToolUse](https://code.claude.com/docs/en/hooks#pretooluse) hook which fires before each bash command execution attempt, and based on a `config.json` file, decides on one of three possible policies: `allow`, `ask` or `deny`. The configuration file contains a list of `allow` and `deny` rules.

When Claude Code wants to execute a bash command, localmost parses all rules and the input command using ShellCheck. Then, it uses the AST representation of both to decide on a policy:

1. First, it breaks the input command into subcommands. For example, `echo | foo; bar` contains three bash subcommands: `echo`, `foo` and `bar`.
2. For each one, it checks for matches against all the `deny` and `allow` rules.
3. If **any** subcommand is denied, then the resulting policy is `deny`. If **all** subcommands are allowed, then the result is `allow`. Otherwise, the result is `ask`.

It supports a **subset** of bash, falling back to `ask` where necessary. For example, a command with an expression like `$var` will always result in `ask`.

## Installation

...

Ensure that localmost has been installed correctly:
```
localmost --help
```

Finally, place the following in your Claude Code's `settings.json` file:
```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [{"type": "command", "command": "localmost check"}]
      }
    ]
  }
}
```

This will instruct Claude Code to use localmost to decide on bash command execution.

## Configuration

Localmost is configured via a single `config.json` file. You can create an empty one using the `init` subcommand:

```bash
localmost init
```

The top level keys for the file are `allow` and `deny`, each one containing a list of rules. Here's a quick example:

```json
{
  "allow": [
    {"rule": "echo @arg*"}
    {"rule": "mkdir @(-p)? @path"},
    {"rule": "ls @{-l,-a}* @path"},
    {"rule": "head @*"},
    {"rule": "find @*", "unless": ["-exec", "-delete"]}
  ],
  "deny": [
    {"rule": "rm @arg*"}
  ]
}
```

Rules are written using a special syntax, which is syntatically still bash, but the meaning of some expressions is changed. It allows using "meta expressions" to represent a set of expressions that might appear in input commands. The rule text itself must be placed in the `rule` key, for each rule.

Here's a full overview of the rules syntax:

| Expression    | Meaning                                                                                                                                           |
|---------------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| `abc`         | A literal string `abc` will match `abc`, `'abc'` and `"abc"`.                                                                                     |
| `@arg`        | Matches any argument. In `foo -a --xyz --test=bar baz --`, `-a`, `--xyz`, `--test=bar`, `baz` and `--` are each a separate argument.              |
| `@path`       | Matches an argument that contains a valid path, in terms of allowed characters. For example, in Linux, `NUL` characters are not allowed in paths. |
| `@int`        | Matches an argument containing an integer value, e.g. `1234`.                                                                                     |
| `@@`          | Matches a literal `@` character.                                                                                                                  |
| `@{v1,v2,v3}` | Choice: matches any of `v1`, `v2` or `v3`.                                                                                                        |
| `@(v1 v2 v3)` | Group: matches `v1 v2 v3` in that specific order.                                                                                                 |

In addition to that, expressions (except literals) can also have quantifiers:

| Quantifier | Meaning                              |
|------------|--------------------------------------|
| `?`        | Zero or one time. E.g. `@arg?`.      |
| `+`        | One or more times. E.g. `@{-a,-b}+`. |
| `*`        | Zero or more times. E.g. `@int*`     |

> [!TIP]
> As a special case, `@*` is a shortcut for `@arg*`, allowing you to write e.g. `echo @*`.

Additionally, each rule can also set the following keys:

- `unless`: List of expressions that **must not** appear anywhere in the input subcommand in order for the rule to match. This is particularly useful for commands where only a few flags could be considered problematic, so one can set the rule to `foo @*` and then "un-match" some flags.
- `redirect`: Can be `true`, `false` or `"safe"` (default: `"safe"`).
  - `true` implies that the rule matches no matter which redirects the input sucommand has.
  - `false` implies that the rule only matches if the input subcommands has no redirects.
  - `"safe"` implies that the rule only matches if the input subcommand has only "safe" redirects, e.g. `> /dev/null`.
- `pipe`: Can be `true`, `false`, `"in"` or `"out"`.

## Tips

- Since localmost is an external process, you don't need to reload your (e.g. Claude Code) session in order for any configuration changes to be picked up.
