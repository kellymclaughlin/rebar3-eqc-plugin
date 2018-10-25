A rebar3 plugin to enable the execution of Erlang QuickCheck properties

## Description

This plugin provides the ability to run all or a subset of the EQC
properties defined in a project's applications with a simple rebar3
command. For a property to be found and executed by the plugin it must
use the `prop_*` naming convention and be exported from the module in
which it resides. Otherwise, the property name may be given using the
`--properties` option. The number of times each property is executed
is configurable as shown in the `Configuration` and `Usage` sections
below with the default being 100 times. For users of the full version
of EQC it is also possible to have each property execute for a certain
period of time as opposed to a fixed number of iterations per
property.

The plugin takes the approach of executing EQC properties directly as
opposed to the common pattern of indirect execution via another test
framework. There are still cases where it may be useful to use eunit
or common_test for execution of EQC properties, but there are many
cases where that is not required.

The plugin scans all source modules for EQC properties. Additionally
it looks for standalone EQC properties in a top-level `eqc` directory.

The default plugin behavior is to execute all properties found in the
application source code or the `eqc` directory. This behavior can be overridden
by using the `--properties` option to specify a comma-separated list of
properties to be executed. List entries may be of the form
`ModuleName:PropertyName` or simply `PropertyName` if the property name is
unique within the project. Additionally, as of version 1.1.0 there is a `--dir`
option to override the default test directory location of `eqc`.

The plugin also has a counterexample mode. When a property fails when
run using the plugin a counterexample file is written to an `.eqc`
sub-directory in the project directory. The counterexample files are
named for the properties that generate them. This allows for
rechecking a set of properties using their respective counterexample
files except in cases where a project has properties sharing the same
name in different modules. The `--properties` option may also be used
with counterexample mode to recheck a specific property or set of
properties.

## Installation

To install the plugin using the Hex package manaage, add the following
to the rebar.config file:

```
{plugins, [rebar3_eqc]}
```

Alternatively, to install the plugin from github use the following:


```
{plugins, [
    {rebar3_eqc, ".*", {git, "https://github.com/kellymclaughlin/rebar3-eqc-plugin.git", {tag, "1.0.0"}}}
]}.

```

To set the number of test executions to 500 instead of the default of
100, add the following rebar.config entry:

```
{eqc_opts, [{numtests, 500}]}.
```

To specify that each property execute for 30 seconds, use this entry instead:

```
{eqc_opts, [{testing_time, 30}]}.
```

The `numtests` and `testing_time` options are mutually exclusive. If
both are specified, the `testing_time` setting is ignored.

## Usage

To view the plugin usage menu with a full list of options and their
descriptions use `./rebar3 help eqc`. The following are some example
scenarios of plugin usage.

Execute each property the configured number of times or for the
configured duration:

```
./rebar3 eqc
```

Override the configuration in `rebar.config` or the default and
execute each property 10 times:

```
./rebar3 eqc -n 10
```

Similarly, override the configuration in `rebar.config` or the default
and execute each property for 45 seconds:

```
./rebar3 eqc -t 45
```

To only execute the property `prop_test1` in the module `test_module`
for 1000 iterations use the following:

```
./rebar3 eqc -n 1000 -p test_module:prop_test1
```

To execute the same test, but to have the plugin determine the module
name use the following:

```
./rebar3 eqc -n 1000 -p prop_test1
```
