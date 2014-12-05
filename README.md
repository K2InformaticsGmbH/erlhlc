erlhlc
======

erlhlc - Hybrid Logical Clock cluster application.

A distribution and cluster synchronization wrapper over https://github.com/k2informatics/hlc

`application:start(erlhcl)` or `erlhcl:start/0` starts a locally registered process `erlhcl_sync` as a node local source of time. `erlhcl:next_now/0` can replace `erlang:now/0` for a est effort cluster consistant timestamp.

Public APIs are:
```erlang
erlhcl:last_now() -> erlang:timestamp().
erlhcl:next_now() -> erlang:timestamp().
erlhcl:last_ts() -> hlc:timestamp().
erlhcl:next_ts() -> hlc:timestamp().
```

`last_*` variants only returns the last used clock and it doesn't change the internal clock

`hlc:timestamp()` is defined as `#timestamp{wall_time,logical}`
