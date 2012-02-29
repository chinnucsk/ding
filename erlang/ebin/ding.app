{application, ding,
 [{description,"Ding IRC Bot."},
  {vsn,"0.1"},
  {modules, [ding,ding_sup,girc,bot,ircmsg,connectionhelper]},
  {registered, [ding]},
  {applications, [kernel, stdlib]},
  {env, []},
  {mod, {ding,[]}}]}.