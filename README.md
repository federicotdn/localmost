# localmost

A flexible and deterministic Claude Code `PreToolUse` tool based on [ShellCheck](https://www.shellcheck.net/).

Localmost uses the [PreToolUse](https://code.claude.com/docs/en/hooks#pretooluse) hook which fires before each bash command execution attempt, and based on a `config.json` file, decides on one of three possible policies: `allow`, `ask` or `deny`. It supports a **subset** of bash, falling back to `ask` where necessary.

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

...

## Tips

- Since localmost is an external process, you don't need to reload your (e.g. Claude Code) session in order for any configuration changes to be picked up.
