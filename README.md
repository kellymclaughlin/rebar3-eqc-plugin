A rebar3 plugin to enable the execution of Erlang QuickCheck properties

## Description

Right now this is a very basic plugin. It provides the ability to run
all of the properties defined in a project's applications using
`eqc:module/1`. This means each property must use the `prop_*` naming
convention and each property is only executed the default of 100
times.

More interesting capabilities will be added soon. These may include,
but are not limited to the following:

* Provide options to only run the properties from specific apps or modules.
* Provide an option to only run a particular list of properties.
* Provide a way to customize the number of times properties are executed.
* Provide a way to use `eqc:testing_time` instead of `eqc:numtests`for
property execution.
* Provide a way to re-test a property using a saved counterexample.
* Add an option to just set the EQC compiler directive and then do a
normal eunit test execution.

## Configuration

Configure the plugin by adding the following
to the rebar.config file:

```
{plugins, [
    {rebar_prv_eqc, ".*", {git, "https://github.com/kellymclaughlin/rebar_prv_eqc.git", {branch, "master"}}}
]}.

```

## Usage

```
./rebar3 eqc
```
