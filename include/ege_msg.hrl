-ifndef(BG_MSG_H).
-define(BG_MSG_H, true).

-include_lib("kernel/include/logger.hrl").

%% 错误等级
-define(E_DEBUG,        1).
-define(E_INFO,         2).
-define(E_NOTICE,       4).
-define(E_WARNING,      8).
-define(E_ERROR,        16).
-define(E_CRITICAL,     32).
-define(E_ALERT,        64).
-define(E_EMERGENCY,    128).

-define(CMD_ERR(Reason, ErrLvl), #{error => Reason, err_lvl => ErrLvl}).

-define(EGEN_LEVEL2NUM(Level),
    case Level of
        debug -> ?E_DEBUG;
        info -> ?E_INFO;
        notice -> ?E_NOTICE;
        warning -> ?E_WARNING;
        error -> ?E_ERROR;
        critical -> ?E_CRITICAL;
        alert -> ?E_ALERT;
        emergency -> ?E_EMERGENCY
    end).

-define(EGEN_NUM2LEVEL(Num),
    case Num of
        ?E_DEBUG -> debug;
        ?E_INFO -> info;
        ?E_NOTICE -> notice;
        ?E_WARNING -> warning;
        ?E_ERROR -> error;
        ?E_CRITICAL -> critical;
        ?E_ALERT -> alert;
        ?E_EMERGENCY -> emergency
    end).

-record(net_handler, {
    role_pid :: pid(), % 角色进程
    interval_config_key = msg_interval_config :: term(), % 获取消息间隔配置key
    err_times = 0 :: non_neg_integer(), % 错误次数
    last_cmd_time = 0 :: non_neg_integer(), % 最后发送请求时间 second
    last_heartbeat :: non_neg_integer(), % 最后发送心跳时间 milli second
    ip :: binary()
}).

-endif.