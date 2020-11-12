# Rentier

Free and open source online booking software for small and medium enterprises.

## Environment variables

- FIREBASE_API_KEY
- FIREBASE_PROJECT_ID
- FIREBASE_MSG_SENDER_ID

## Development

Docker is the only thing required to get started.

```shell
# start nix shell
./nix/shell.sh

# open IDE
vi .

# start dev server on IDE terminal buffer
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost. If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Tests

```shell
stack test --flag rentier:library-only --flag rentier:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).
