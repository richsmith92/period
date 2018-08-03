## Period

https://hackage.haskell.org/package/period

Library and executable to parse and format date periods, collapse and expand, e.g. 2018 → 2018-01-01,2018-12-31 and backwards.

```
Usage: period COMMAND
  Expand or collapse text representation of date range.

  period collapse 2018-01-01,2018-12-31
  => 2018

  period collapse 2018-01-01,2118-12-31
  => 2018,2118

  period expand 2018-01
  => 2018-01-01,2018-01-31

  period expand 2018-01-07,14
  => 2018-01-07,2018-01-14

Available options:
  -h,--help                Show this help text

Available commands:
  collapse                 Find shortest representation for time period
  expand                   Display start and end points of time period

```
