You need the stack servant tool that can be downloaded like this in Unix systems:

``` bash
curl -sSL https://get.haskellstack.org/ | sh
```
For further instructions about Stack please go to https://docs.haskellstack.org/en/stable/README/

This is a very minimal example of a project that uses

- `servant` to specify a REST API,
- `servant-server` to implement a server,
- `hspec` and `servant-client` for the test-suite.
- `persistent` for database management.
- `servant-auth` and `servant-auth-server` for JWT authentication.

To set up the project and run the test-suite, do:

``` bash
stack setup
stack test --fast
```

to reload the code (after making changes) and run the tests again.

To run the app, do:

``` bash
stack build
stack exec servant-persistent-template
```

I tried to create a Clean architecture, complete domain testing and split commands/queries to make it easy to extend in the future.
