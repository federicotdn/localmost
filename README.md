# localmost

A flexible and deterministic Claude Code `PreToolUse` tool based on [ShellCheck](https://www.shellcheck.net/).

Features:
- Write permission rules using a regular expression-like syntax.
- Correctly parses complex commands using `|`, `;`, `if`, `for`, `&&`, `||`, `>` and more.
- Configure file redirections per command.
- Configure ability to pipe in and/or out per command as well.
- Automatically allows safe `xargs` commands to be executed.

> [!WARNING]
> Localmost is still at a very early stage of development, and works best under the assumption that Claude Code is not actively trying to execute destructive bash commands.

## How it Works

Localmost uses the [PreToolUse](https://code.claude.com/docs/en/hooks#pretooluse) hook which fires before each bash command execution attempt, and based on a `config.json` file, decides on one of three possible policies: `allow`, `ask` or `deny`. The configuration file contains a list of `allow` and `deny` rules.

When Claude Code wants to execute a bash command, localmost parses all rules and the input command using ShellCheck. Then, it uses their AST representation to decide on a policy:

1. First, it breaks the input command into subcommands. For example, `echo | foo; bar` contains three bash subcommands: `echo`, `foo` and `bar`.
2. For each one, it checks for matches against all the `deny` and `allow` rules. If any `deny` rule matches, the subcommand is denied. Otherwise, if any `allow` rule matches, the subcommand is allowed.
3. Finally, if **any** subcommand is denied, then the resulting policy for the input command is `deny`. If **all** subcommands are allowed, then the result is `allow`. Otherwise, the result is `ask`.

As a note, localmost supports only a **subset** of bash, falling back to `ask` where necessary. For example, a command with an expression like `$var` will always result in `ask`.

The largest advantage of using ShellCheck is that it gives localmost the ability to parse complex command sequences, even when they contain pipes, redirects, `if` statements, `for` loops, etc. This is much more reliable than say a regular expression-based approach.

## Installation

**Pre-built binaries:**
1. Head over to the [releases](https://github.com/federicotdn/localmost/releases) section.
2. Download the latest one for your OS and CPU architecture, and place it somewhere in `$PATH`.

Note: Windows is not supported yet.

**From source:**
1. Install [ghcup](https://www.haskell.org/ghcup/).
2. Install GHC and Cabal using `ghcup`. See [release.yaml](.github/workflows/release.yaml) for the exact versions.
3. Clone this repo: `git clone git@github.com:federicotdn/localmost.git && cd localmost`.
4. Run `make install` (assumes `.ghcup/env` shim is set up).

Afterwards, ensure that localmost has been installed correctly:
```
localmost --version
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

this will instruct Claude Code to use localmost to decide on bash commands execution.

## Configuration

Localmost is configured via a single `config.json` file. You can create an empty one using the `init` subcommand:

```bash
localmost init
```

Here's a quick example of what a customized configuration file could look like:

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

You can always validate your configuration file by running `localmost config validate`. If your configuration file is not valid or is not present, localmost will default to the `ask` policy.

Following is an explanation of each top-level key for `config.json`:

### `allow` and `deny`

These two keys can contain a list of rules.

Rules are written using a special syntax, which is syntatically still bash, but the meaning of some expressions is changed. It allows using "meta expressions" to represent a set of expressions that might appear in input commands. The rule text itself must be placed in the `rule` key, for each rule.

Here's a full overview of the rules syntax:

| Meta expression | Meaning                                                                                                                                           |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------|
| `abc`           | Not really a meta expression per se. A literal string `abc` will match `abc`, `'abc'` and `"abc"`.                                                |
| `@arg`          | Matches any argument. In `foo -a --xyz --test=bar baz --`, `-a`, `--xyz`, `--test=bar`, `baz` and `--` are each a separate argument.              |
| `@path`         | Matches an argument that contains a valid path, in terms of allowed characters. For example, in Linux, `NUL` characters are not allowed in paths. |
| `@int`          | Matches an argument containing an integer value, e.g. `1234`.                                                                                     |
| `@@`            | Matches a literal `@` character.                                                                                                                  |
| `@{v1,v2,v3}`   | Choice: matches one of `v1`, `v2` or `v3`.                                                                                                        |
| `@(v1 v2 v3)`   | Group: matches `v1 v2 v3` in that specific order.                                                                                                 |

In addition to that, meta expressions can also have quantifiers:

| Quantifier | Meaning                              |
|------------|--------------------------------------|
| `?`        | Zero or one time. E.g. `@arg?`.      |
| `+`        | One or more times. E.g. `@{-a,-b}+`. |
| `*`        | Zero or more times. E.g. `@int*`     |

> [!TIP]
> As a special case, `@*` is a shortcut for `@arg*`, allowing you to write e.g. `echo @*`.

Rule matching is mechanichally similar to regular expressions, meaning that the order of the expressions matters. For example, the rule `foo -a -b` will match `foo -a -b` but not `foo -b -a`. Although this limits the expressiveness of rules, it helps keeping the matching logic much simpler. Additionally, some commands like `find` may behave differently depending on the order of provided flags, so matching (e.g. allowing) different orders could potentially be incorrect. This depends entirely on the flag semantics of each command, which localmost is not aware of in any way.

Additionally, each rule can also set the following keys:

**`unless`**

List of expressions that **must not** appear anywhere in the input subcommand in order for the rule to match. This is particularly useful for commands where only a few flags could be considered problematic, so one can set the rule to `foo @*` and then "un-match" some flags. Defaults to an empty list.

**`redirect`**

Can be `true`, `false` or `"safe"` (default: `"safe"`):
- `true` implies that the rule matches no matter which redirects the input sucommand has.
- `false` implies that the rule only matches if the input subcommands has no redirects.
- `"safe"` implies that the rule only matches if the input subcommand has only "safe", non-destructive redirects, e.g. `> /dev/null`.

**`pipe`**

Can be `true`, `false`, `"in"` or `"out"` (default: `true`):
- `true` will make the rule match without considering if the subcommand is part of a pipeline.
- `false` will make the rule match only if the subcommand is not part of a pipeline.
- `"in"` will make the rule match only if the subcommand is not part of a pipeline, or if it appears at the very end of one.
- `"out"` will make the rule match only if the subcommand is not part of a pipeline, or if it appears at the beginning of one.

### `allowSafeXargs`

Can be set to `true` or `false` (default: `true`).

When set to `true`, commands in the shape of `echo ARGS | xargs PROG` will be marked as allowed if and only if checking for `PROG ARGS` would result in an `allow` policy. This feature also requires having an `allow` rule in place equivalent to `echo @arg*` (in order to allow for the `echo` to run).

If set to `false`, no special processing will be done for these situations.

> [!WARNING]
> Adding manual `allow` rules for `xargs` is not recommended, since `xargs` will read its arguments from `stdin`, which is not something a rule can operate on in any way.

## Usage

Run `localmost --help` to see usage help.

You can call `localmost check` manually in order to test out command policies:

```bash
echo 'ls -a' | localmost check --mode text
```

## Tips

- Since localmost is an external process, you don't need to reload your Claude Code session in order for any configuration changes to be picked up.
- Only use `@arg*`/`@*` for commands that you are very familiar with, and are sure that cannot be used in a destructive way, e.g. `echo` or `ls`. If you do use `@arg`/`@*`, consider adding `unless` clauses as well in order to un-match specific flags.
- An `unless` value of `["-a", "-b", "-c"]` can also be written as `["@{-a,-b,-c}"]`, making it a bit more compact.
- See the [examples.md](docs/examples.md) file for some configuration file examples.

## Related

- [lord-kali](https://github.com/insidewhy/lord-kali): A similar tool built using Rust and `tree-sitter-bash`.

## License

Distributed under the GNU General Public License, version 3.

See [LICENSE](LICENSE) for more information.
