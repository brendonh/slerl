{application, slerl,
    [{description, "slerl"},
     {vsn, "0.1"},
     {modules, [slerl_app]},
     {registered, []},
     {applications, [kernel, stdlib]},
     {mod, {slerl_app, []}},
     {env, []},
     {start_phases, []}
]}.