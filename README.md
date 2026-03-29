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
    {"rule": "echo @*"}
    {"rule": "git @(-C @path)? @{log,status,diff,show,blame} @*"},
    {
      "rule": "find @*",
      "unless": ["@{-delete,-fprint,-fprint0,-fprintf,-fls,-exec,-execdir,-ok,-okdir}"]
    }
  ]
}
```

If a command matches a `deny` rule, it is denied. If it matches instead an `allow` rule, it is allowed. If it matches none, the result is `ask`.

## Tips

- Since localmost is an external process, you don't need to reload your (e.g. Claude Code) session in order for any configuration changes to be picked up.
