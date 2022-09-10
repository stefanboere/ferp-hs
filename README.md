# Ferp-hs

_Ferp-hs is still under active development and not at all production ready._

Functional reactive web applications using `reflex-dom` and `servant`.
Our aim is to make it easier to write large, non public facing, CRUD applications,
such as administration consoles, site administration pages of CMS's and
other internal applications.
We aim to make it easy to add the standard pages needed for CRUD applications,
such as tables and edit forms, while still remaining extensible if some parts
do not fit this exactly.

For a showcase of the frontend components see <https://stefanboere.github.io>.

The core libraries are:

| Component                   | Description                                                                                                                    |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `backend`                   | The application which serves the html (prerendered) and javascript                                                             |
| `backend-api`               | The rest api and websocket handler                                                                                             |
| `backend-extra`             | Common utilities used by both `backend` and `backend-api`                                                                      |
| `beam-crud`                 | Helpers for writing common CRUD queries with [beam](https://haskell-beam.github.io/beam)                                       |
| `frontend`                  | [reflex-dom](https://github.com/reflex-frp/reflex-dom) components based on the [Clarity](https://clarity.design) design system |
| `keycloak-nordtheme`        | A [nordtheme](https://nordtheme.com) for the [Keycloak](https://www.keycloak.org) Identity and Access Management system        |
| `project-m36-crud`          | Helpers for writing common CRUD queries with [project-m36](https://github.com/agentm/project-m36)                              |
| `servant-ac`                | Type-level DSL for specifying role based access control rules for [servant](https://docs.servant.dev) API's                    |
| `servant-ac-server`         | Server implementation of `servant-ac`                                                                                          |
| `servant-crud`              | Filtering and sorting url parameters extension to servant                                                                      |
| `servant-crud-server`       | Server implementation of `servant-crud-server`                                                                                 |
| `servant-subscriber-reflex` | Connect to [servant-subscriber](https://github.com/eskimor/servant-subscriber) from `reflex-dom`                               |

## Getting started

To run locally, first install the `nix` package manager with flake support.
Then enter the development shell with `nix-shell --pure` or `nix develop`.

There are three main executables to be run, namely `frontend`, `backend` and `backend-api`.

`frontend` uses jsaddle-warp to serve the frontend on `localhost:3003` (using ghc).
It can also build the .js files if ghcjs as compiler is used.
To run, cd into the frontend directory and enter a cabal repl.

```sh
cd ./frontend && cabal repl
```

Once in the repl type in `mainWithHead` and hit enter.
If you made changes to `frontend` you can just hit CTRL-C, then reload `:r`,
run `mainWithHead` again and refresh the page <http://localhost:3003>.

`backend` can be run as follows

```sh
cd ./backend && cabal run backend:exes
```

or

```sh
cd ./backend && nix run .\#backend
```

It is also possible to run it from within a cabal repl.
`backend` serves the prerendered html and compiled javascript and css files and
is therefore usually not needed for development.
One exception is for authentication, this is also handled by `backend`.

`backend-api` is run similarly

```sh
cd ./backend-api && cabal run backend-api:exes
```

or

```sh
cd ./backend-api && nix run .\#backend-api
```

### Vendor javascript files

Some javascript libraries are used in this project, for example ace and mathjax.
To create a directory with all these files in the right location, run

```sh
nix build .\#vendor-lib -o result-vendor
```

Serve `result-vendor` on localhost:8081 with

```sh
http-server -cors localhost
```

Now the jsaddle-warp frontend can access the vendor files (as configured in `frontend/config.json`).

Similarly the minified frontend can be build with

```sh
nix build .\#frontend-min -o result-frontend-min
```

These files are served by `backend` as configured in `backend/config.dhall`
(the setting `staticDirectory`).

### Database

A posgresql database is needed for `backend-api`.
The connection information is configured in `backend-api/config.dhall`.
An empty database with sufficient permissions is enough,
the migration will then be done automatically.

### Authentication

For authentication we need an OpenID Connect provider, such as keycloak.
The providerUri and clientId and clientSecret need to be configured in
both `backend/config.dhall` as `backend-api/config.dhall`.

In keycloak, create a new realm called `ferp-hs`.
In this realm, add a new client with clientId `ferp-hs-backend` and
the following options

```json
{
  "clientId": "ferp-hs-backend",
  "enabled": true,
  "clientAuthenticatorType": "client-secret",
  "redirectUris": [
    "http://localhost:3007/",
    "http://localhost:3007/auth/return"
  ],
  "webOrigins": ["http://localhost:3007"],
  "publicClient": false,
  "protocol": "openid-connect"
}
```

This json can be imported into keycloak. If using the UI,
the option `clientAuthenticatorType` is called `Access Type`.
Then go to the Credentials tab and add a client secret
(Client Authenticator: Client id and Secret).
Save this client secret in the file `/run/secrets/oidc-client-secret`
(as configured in `backend/config.dhall`).

Create a new user account in the realm.
Finally add the role `administrator` and add the user to this role.

Now you can test by running the backend executable and opening <http://localhost:3007>.
The login button is in the top right corner.
The jsaddle-warp frontend can use the same cookie as backend,
so you are now also logged in on <http://localhost:3003>.

## Known issues

- `cabal run backend-api:exes` is currently does not run on my system due to a GLIBC version error.
  This should hopefully be solved when <https://github.com/reflex-frp/reflex-platform/pull/768> merges.
  Until then it can be solved by not using `direnv` and running `nix run .\#backend-api`.
- `backend` and `backend-api` cannot start if the OpenID Connect server is unavailable.
  This restriction should be lifted to allow easier deployment and development.
- Setting up a local development with database and keycloak is quite difficult.
- Some reflex-dom components (especially the combobox) are quite slow

## Features

- REST api using servant with sorting, filtering and pagination.
- Authentication using OpenID connect identity provider.
- Specify Access control rules in servant types.
- Reflex components based on the clarity design system, including the datagrid.
- Server side rendering (prerendering)
- Live updates using `servant-subscriber` and websockets.
