#Erlang Reloader

本项目是基于[the MochiWeb reloader](https://github.com/mochi/mochiweb/blob/master/src/reloader.erl).
它可以监控每个beam文件的变更，从而进行加载，做法比较粗暴，每隔一段时间检查有哪些beam是需要重新加载。这个时间之前是写死在代码，我将其放在state里面，并且可以动态去更改它，也可以通过app的env来设置。同时，我将io:format换成了error_logger，这样方便线上项目知道reloader有没有正常工作。使用本项目的话，就不需要依赖mochiweb任何文件了。

##Installation

在你的`rebar.config`添加:

    {reloader, ".*", {git, "https://github.com/roowe/reloader", "master"}}

之后执行 `rebar get-deps`接着 `rebar compile`.

##Usage

在你的应用里面启动reloader :

    application:start(reloader)

SASL output omitted ...

    Eshell V5.9.3  (abort with ^G)
    (gs_admin@127.0.0.1)1>

After recompiling `nuclear_ant_server.erl`:

    =INFO REPORT==== 18-Jun-2014::17:47:01 ===
    Reloading reloader ...
    =INFO REPORT==== 18-Jun-2014::17:47:01 ===
    reload reloader ok.


