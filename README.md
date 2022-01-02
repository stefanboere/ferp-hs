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
| `servant-ac`                | Type-level DSL for specifying role based access control rules for [servant](https://docs.servant.dev) API's                    |
| `servant-ac-server`         | Server implementation of `servant-ac`                                                                                          |
| `servant-crud`              | Filtering and sorting url parameters extension to servant                                                                      |
| `servant-crud-server`       | Server implementation of `servant-crud-server`                                                                                 |
| `servant-subscriber-reflex` | Connect to [servant-subscriber](https://github.com/eskimor/servant-subscriber) from `reflex-dom`                               |
