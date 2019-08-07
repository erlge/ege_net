-ifndef(EGE_NET_H).
-define(EGE_NET_H, true).

-include_lib("kernel/include/logger.hrl").
-include("ege_msg.hrl").

-define(APP_NAME, ege_net).

%% 协议路由模块
-ifndef(MSG_ROUTER_MODULE).
-define(MSG_ROUTER_MODULE, msg_router).
-endif.

-define(TCP_TIMEOUT_SHUTDOWN, 5000). % 5 seconds

%% heartbeat kickout timeout
-ifndef(HEARTBEAT_KICKOUT_TIMEOUT).
-define(HEARTBEAT_KICKOUT_TIMEOUT, 5000). %% 5 secs
-endif.

%% custom heap_size switch
-ifndef(HANDLER_CUSTOM_HEAP_SIZE_SWITCH).
-define(HANDLER_CUSTOM_HEAP_SIZE_SWITCH, true).
-endif.

%% process_flag min_heap_size
-ifndef(HANDLER_MIN_HEAP_SIZE).
-define(HANDLER_MIN_HEAP_SIZE, 65536 * 2).
-endif.

%% process_flag min_bin_vheap_size
-ifndef(HANDLER_MIN_BIN_VHEAP_SIZE).
-define(HANDLER_MIN_BIN_VHEAP_SIZE, 65536 * 2).
-endif.

-define(CHECK_TICK(NetHandler, NowMs),
    NetHandler#net_handler.last_heartbeat + ?HEARTBEAT_KICKOUT_TIMEOUT =< NowMs).

-define(HEARTBEAT_TIMEOUT_REASON, heartbeat_kickout_timeout).

%% 最大错误等级
-ifndef(MAX_ERR_LVL).
-define(MAX_ERR_LVL, 30).
-endif.

%% 最大未登录可以处理的协议号
%% 大于这个协议号的都会分发到角色进程处理
%% 小于等于这个协议号的协议在socket进程处理
-ifndef(MAX_NO_LOGIN_CMD).
-compile({parse_transform, ct_expand}).
-define(MAX_NO_LOGIN_CMD, ct_expand:term((20 + 1) bsl 9 - 1)).
-endif.

-define(DEFAULT_INTERVAL_CONFIG, [
    {0, {200, {false, ?E_NOTICE}}},
    {default, {100, {false, ?E_INFO}}}
]).

-endif.