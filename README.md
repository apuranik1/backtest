Backtest
========

A bunch of R code for writing backtests.

Quickstart
----------

Copy `odbcparams.r.template` to something more like `odbcparams.r` and fill in the relevant fields.
Now you can connect to obelix from the R shell with

```
dbhandle <- obelix.connect('odbcparams.r')
```

That's all the code there is for now, so good luck with the rest of the backtesting.
