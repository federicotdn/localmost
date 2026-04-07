# Localmost Changelog
## **0.1.0.1** - 2026-04-07
- Fixed `text` format for `localmost check` not reading all stdin lines.
- Fixed crash when running bash commands with zero internal subcommands (e.g. `(( 1 + 1))`).
- Tweaked contents of initial configuration file created by `localmost init`.
- Improved safe `xargs` feature, allowing for some arguments to be passed directly to `xargs` itself (e.g. `echo foo | xargs rgrep -v`).

## **0.1.0.0** - 2026-03-30
- Initial release.
