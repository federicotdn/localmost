# Localmost Changelog
## **0.1.0.2** - 2026-04-23
- Allow more commands to run with `xargs`, provided that the appropriate rules exist.
- Allow `@arg` to match with non-literal expressions that are guaranteed to expand to a single argument (e.g. `"$var"`).

## **0.1.0.1** - 2026-04-07
- Fixed `text` format for `localmost check` not reading all stdin lines.
- Fixed crash when running bash commands with zero internal subcommands (e.g. `(( 1 + 1))`).
- Tweaked contents of initial configuration file created by `localmost init`.
- Improved safe `xargs` feature, allowing for some arguments to be passed directly to `xargs` itself (e.g. `echo foo | xargs rgrep -v`).

## **0.1.0.0** - 2026-03-30
- Initial release.
