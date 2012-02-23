{application, ding,
 [{description,"Ding IRC Bot."},
  {vsn,"0.1"},
  {modules, [ding,ding_sup,girc,bot,ircmsg]},
  {registered, [ding]},
  {applications, [kernel, stdlib, inets]},
  {env, []},
  {mod, {ding,[]}}]}.