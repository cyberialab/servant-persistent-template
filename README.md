You need the stack servant tool that can be downloaded like this in Unix systems:

``` bash
curl -sSL https://get.haskellstack.org/ | sh
```
For further instructions about Stack please go to https://docs.haskellstack.org/en/stable/README/

This is a very minimal example of a project that uses

- `servant` to specify a REST API,
- `servant-server` to implement a server,
- `hspec` and `servant-client` for the test-suite.

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
