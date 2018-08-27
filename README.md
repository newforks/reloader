#Erlang Reloader

本项目是基于[the MochiWeb reloader](https://github.com/mochi/mochiweb/blob/master/src/reloader.erl).
它可以监控每个beam文件的变更，从而进行加载，每隔一段时间检查有哪些beam是需要重新加载。
间隔时间可以通过以下两种方式修改:

1. 修改env选项:check_time，单位是秒
2. 通过命令 ``reloader:set_check_time/1`` 动态修改间隔时间



##Installation

在你的`rebar.config`添加:

    {reloader, {git, "https://github.com/newforks/reloader", "master"}}

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


