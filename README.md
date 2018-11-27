# Erlang Reloader

本项目是基于[the MochiWeb reloader](https://github.com/mochi/mochiweb/blob/master/src/reloader.erl).
它可以监控每个beam文件的变更，从而进行加载，每隔一段时间检查有哪些beam是需要重新加载。
间隔时间可以通过以下两种方式修改:

1. 修改env选项:check_time，单位是秒，默认为5s
2. 通过命令 ``reloader:set_check_time/1`` 动态修改间隔时间

## 状态查看

    erl> {Delay, Date} = reloader:status().
    % 说明:
    % Delay: 多少毫秒执行一次(为0或undefined表示手动)
    % Date: 上次执行时间

## 定时与非定时模式

定时:

    % 每1秒执行一次定时
    {check_time, 1}

非定时:

    % 关闭定时
    {check_time, 0}

## set_check_time/1命令

定时间隔由1秒修改为2秒:

    reloader:set_check_time(2).
    
关闭定时:

    reloader:set_check_time(0).

重新打开定时,并设置为5秒间隔:

    reloader:set_check_time(5).

## 非定时模式加载

命令:

    reloader:reload().
    % 注意:
    %    加载的文件是从上一次加载到现在这段时间修改过的文件

## Installation

在你的`rebar.config`添加:

    {reloader, {git, "https://github.com/newforks/reloader", "master"}}

之后执行 `rebar3 get-deps`接着 `rebar3 compile`.

## Usage

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

注意::


  erl -mode embedded
    1.interactive:默认
    初使只有部分code被加载,剩下code在被使用时动态加载
    2.embedded:
    启动时加载所有modules
