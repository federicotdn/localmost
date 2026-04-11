# Examples

Here you'll find examples of useful localmost configuration files.

Note: these are a work-in-progress, and might not be 100% safe to use.

```json
{
  "allow": [
    {"rule": "echo @*"},
    {"rule": "git @(-C @arg)? @{log,status,diff,show,blame} @*"},
    {"rule": "git @(-C @arg)? branch @*", "unless": ["@{-d,--delete,-D,-f,--force,-m,-M,-c,-C,-r}"]},
    {"rule": "make @{test,build}?"},
    {"rule": "cabal @{test,build}?"},
    {"rule": "notify-send @*"},
    {"rule": "rg @*"},
    {"rule": "ls @*"},
    {"rule": "cat @*"},
    {"rule": "sort @*"},
    {"rule": "cut @*"},
    {"rule": "wc @*"},
    {"rule": "head @*"},
    {"rule": "tail @*"},
    {"rule": "uniq @*"},
    {"rule": "mkdir @(-p)? @path"},
    {"rule": "rmdir @path"},
    {"rule": "sleep @int"},
    {"rule": "man @arg"},
    {"rule": "col @*"},
    {"rule": "tree @path"},
    {"rule": "uv lock @*"},
    {"rule": "uv sync @*"},
    {"rule": "uv run pytest @*"},
    {"rule": "uv run make test @*"},
    {"rule": "localmost @{check,ast} @*"},
    {"rule": "localmost config @{validate,show}"},
    {"rule": "grep @*"},
    {"rule": "tr @*"},
    {"rule": "cd @path"},
    {"rule": "gh pr @{view,list,diff,checks} @*"},
    {"rule": "gh run @{view,list} @*"},
    {"rule": "ghc --version"},
    {"rule": "ghc list @*"},
    {
      "rule": "find @*",
      "unless": ["@{-delete,-fprint,-fprint0,-fprintf,-fls,-exec,-execdir,-ok,-okdir}"]
    }
  ]
}

```
