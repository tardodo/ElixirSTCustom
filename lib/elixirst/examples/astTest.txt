[
  behaviour: GenServer,
  type_specs: {{:handle_call, 3},
   {[:error, :error, {:list, nil}], {:tuple, [:atom, :error, [:number]]}}}
]
%{
  after_verify: [],
  attributes: [
    behaviour: GenServer,
    type_specs: {{:handle_call, 3},
     {[:error, :error, {:list, nil}], {:tuple, [:atom, :error, [:number]]}}}
  ],
  compile_opts: [:debug_info],
  definitions: [
    {{:terminate, 2}, :def,
     [line: 5, file: {"lib/gen_server.ex", 840}, context: GenServer],
     [
       {[line: 5, file: {"lib/gen_server.ex", 840}, context: GenServer],
        [
          {:_reason, [version: 0, line: 840, counter: {Examples.Stack, 7}],
           GenServer},
          {:_state, [version: 1, line: 840, counter: {Examples.Stack, 7}],
           GenServer}
        ], [], :ok}
     ]},
    {{:start_link, 1}, :def, [line: 12],
     [
       {[line: 12], [{:default, [version: 0, line: 12], nil}],
        [
          {{:., [line: 12], [:erlang, :is_list]}, [line: 12],
           [{:default, [version: 0, line: 12], nil}]}
        ],
        {{:., [line: 13], [GenServer, :start_link]}, [line: 13],
         [Examples.Stack, {:default, [version: 0, line: 13], nil}]}}
     ]},
    {{:push, 2}, :def, [line: 20],
     [
       {[line: 20],
        [
          {:pid, [version: 0, line: 20], nil},
          {:element, [version: 1, line: 20], nil}
        ], [],
        {{:., [line: 21], [GenServer, :call]}, [line: 21],
         [
           {:pid, [version: 0, line: 21], nil},
           {:push, {:element, [version: 1, line: 21], nil}}
         ]}}
     ]},
    {{:pop, 1}, :def, [line: 24],
     [
       {[line: 24], [{:pid, [version: 0, line: 24], nil}], [],
        {{:., [line: 25], [GenServer, :call]}, [line: 25],
         [{:pid, [version: 0, line: 25], nil}, {:{}, [line: 25], [:pop]}]}}
     ]},
    {{:init, 1}, :def, [line: 31],
     [
       {[line: 31], [{:stack, [version: 0, line: 31], nil}], [],
        {:ok, {:stack, [version: 0, line: 32], nil}}}
     ]},
    {{:handle_info, 2}, :def,
     [line: 5, file: {"lib/gen_server.ex", 795}, context: GenServer],
     [
       {[line: 5, file: {"lib/gen_server.ex", 795}, context: GenServer],
        [
          {:msg, [version: 0, line: 795, counter: {Examples.Stack, 7}],
           GenServer},
          {:state, [version: 1, line: 795, counter: {Examples.Stack, 7}],
           GenServer}
        ], [],
        {:__block__, [line: 0],
         [
           {:=, [line: 796],
            [
              {:proc, [version: 3, line: 796, counter: {Examples.Stack, 7}],
               GenServer},
              {:case, [line: 797],
               [
                 {{:., [line: 797], [Process, :info]}, [line: 797],
                  [
                    {{:.,
                      [line: 797, context: GenServer, imports: [{0, Kernel}]],
                      [:erlang, :self]},
                     [line: 797, context: GenServer, imports: [{0, Kernel}]],
                     []},
                    :registered_name
                  ]},
                 [
                   do: [
                     {:->, [line: 798],
                      [
                        [{{:_, [line: 798], GenServer}, []}],
                        {{:.,
                          [
                            line: 798,
                            context: GenServer,
                            imports: [{0, Kernel}]
                          ], [:erlang, :self]},
                         [line: 798, context: GenServer, imports: [{0, Kernel}]],
                         []}
                      ]},
                     {:->, [line: 799],
                      [
                        [
                          {{:_, [line: 799], GenServer},
                           {:name,
                            [
                              version: 2,
                              line: 799,
                              counter: {Examples.Stack, 7}
                            ], GenServer}}
                        ],
                        {:name,
                         [version: 2, line: 799, counter: {Examples.Stack, 7}],
                         GenServer}
                      ]}
                   ]
                 ]
               ]}
            ]},
           {{:., [line: 802], [:logger, :error]}, [line: 802],
            [
              {:%{}, [line: 803],
               [
                 label: {GenServer, :no_handle_info},
                 report: {:%{}, [line: 805],
                  [
                    module: Examples.Stack,
                    message: {:msg,
                     [version: 0, line: 807, counter: {Examples.Stack, 7}],
                     GenServer},
                    name: {:proc,
                     [version: 3, line: 808, counter: {Examples.Stack, 7}],
                     GenServer}
                  ]}
               ]},
              {:%{}, [line: 811],
               [
                 domain: [:otp, :elixir],
                 error_logger: {:%{}, [line: 813], [tag: :error_msg]},
                 report_cb: {:&, [line: 814],
                  [
                    {:/, [],
                     [{{:., [], [GenServer, :format_report]}, [], []}, 1]}
                  ]}
               ]}
            ]},
           {:noreply,
            {:state, [version: 1, line: 818, counter: {Examples.Stack, 7}],
             GenServer}}
         ]}}
     ]},
    {{:handle_cast, 2}, :def,
     [line: 5, file: {"lib/gen_server.ex", 822}, context: GenServer],
     [
       {[line: 5, file: {"lib/gen_server.ex", 822}, context: GenServer],
        [
          {:msg, [version: 0, line: 822, counter: {Examples.Stack, 7}],
           GenServer},
          {:state, [version: 1, line: 822, counter: {Examples.Stack, 7}],
           GenServer}
        ], [],
        {:__block__, [line: 0],
         [
           {:=, [line: 823],
            [
              {:proc, [version: 3, line: 823, counter: {Examples.Stack, 7}],
               GenServer},
              {:case, [line: 824],
               [
                 {{:., [line: 824], [Process, :info]}, [line: 824],
                  [
                    {{:.,
                      [line: 824, context: GenServer, imports: [{0, Kernel}]],
                      [:erlang, :self]},
                     [line: 824, context: GenServer, imports: [{0, Kernel}]],
                     []},
                    :registered_name
                  ]},
                 [
                   do: [
                     {:->, [line: 825],
                      [
                        [{{:_, [line: 825], GenServer}, []}],
                        {{:.,
                          [
                            line: 825,
                            context: GenServer,
                            imports: [{0, Kernel}]
                          ], [:erlang, :self]},
                         [line: 825, context: GenServer, imports: [{0, Kernel}]],
                         []}
                      ]},
                     {:->, [line: 826],
                      [
                        [
                          {{:_, [line: 826], GenServer},
                           {:name,
                            [
                              version: 2,
                              line: 826,
                              counter: {Examples.Stack, ...}
                            ], GenServer}}
                        ],
                        {:name,
                         [version: 2, line: 826, counter: {Examples.Stack, 7}],
                         GenServer}
                      ]}
                   ]
                 ]
               ]}
            ]},
           {:case, [line: 830],
            [
              {{:., [line: 830], [:erlang, :phash2]}, [line: 830], [1, 1]},
              [
                do: [
                  {:->, [line: 831],
                   [
                     [0],
                     {{:., [line: 832], [:erlang, :error]}, [line: 832],
                      [
                        {{:., [line: 832], [RuntimeError, :exception]},
                         [line: 832],
                         [
                           {:<<>>, [alignment: 0, line: 832],
                            [{:"::", ...}, {...}, ...]}
                         ]},
                        :none,
                        [error_info: {:%{}, [line: 832], [module: Exception]}]
                      ]}
                   ]},
                  {:->, [line: 834],
                   [
                     [1],
                     {:{}, [line: 835],
                      [
                        :stop,
                        {:bad_cast,
                         {:msg, [version: 0, line: 835, counter: {...}],
                          GenServer}},
                        {:state,
                         [version: 1, line: 835, counter: {Examples.Stack, ...}],
                         GenServer}
                      ]}
                   ]}
                ]
              ]
            ]}
         ]}}
     ]},
    {{:handle_call, 3}, :def, [line: 50],
     [
       {[line: 50],
        [
          {:req, [version: 0, line: 50], nil},
          {:_from, [version: 1, line: 50], nil},
          []
        ], [],
        {:case, [line: 51],
         [
           {:req, [version: 0, line: 51], nil},
           [
             do: [
               {:->, [line: 52],
                [
                  [push: {:element, [version: 2, line: 52], nil}],
                  {:{}, [line: 52],
                   [:reply, "pushed", [{:element, [version: 2, line: 52], nil}]]}
                ]},
               {:->, [line: 53],
                [
                  [{:{}, [line: 53], [{:_, [line: 53], nil}]}],
                  {:{}, [line: 53], [:reply, "error", []]}
                ]}
             ]
           ]
         ]}}
     ]},
    {{:code_change, 3}, :def,
     [line: 5, file: {"lib/gen_server.ex", 845}, context: GenServer],
     [
       {[line: 5, file: {"lib/gen_server.ex", 845}, context: GenServer],
        [
          {:_old, [version: 0, line: 845, counter: {Examples.Stack, 7}],
           GenServer},
          {:state, [version: 1, line: 845, counter: {Examples.Stack, 7}],
           GenServer},
          {:_extra, [version: 2, line: 845, counter: {Examples.Stack, 7}],
           GenServer}
        ], [],
        {:ok,
         {:state, [version: 1, line: 846, counter: {Examples.Stack, 7}],
          GenServer}}}
     ]},
    {{:child_spec, 1}, :def,
     [line: 5, file: {"lib/gen_server.ex", 762}, context: GenServer],
     [
       {[line: 5, file: {"lib/gen_server.ex", 762}, context: GenServer],
        [
          {:init_arg, [version: 0, line: 762, counter: {Examples.Stack, 7}],
           GenServer}
        ], [],
        {:__block__, [line: 0],
         [
           {:=, [line: 763],
            [
              {:default, [version: 1, line: 763, counter: {Examples.Stack, 7}],
               GenServer},
              {:%{}, [line: 763],
               [
                 id: Examples.Stack,
                 start: {:{}, [line: 765],
                  [
                    Examples.Stack,
                    :start_link,
                    [
                      {:init_arg,
                       [version: 0, line: 765, counter: {Examples.Stack, ...}],
                       GenServer}
                    ]
                  ]}
               ]}
            ]},
           {{:., [line: 768], [Supervisor, :child_spec]}, [line: 768],
            [
              {:default, [version: 1, line: 768, counter: {Examples.Stack, 7}],
               GenServer},
              []
            ]}
         ]}}
     ]}
  ],
  deprecated: [],
  file: "c:/Users/farad/Documents/ElixirPlayground/ElixirST/lib/elixirst/examples/stack.ex",
  is_behaviour: false,
  line: 1,
  module: Examples.Stack,
  relative_file: "lib/elixirst/examples/stack.ex",
  struct: nil,
  unreachable: []
}
[
  %{
    after_verify: [],
    attributes: [
      behaviour: GenServer,
      type_specs: {{:handle_call, 3},
       {[:error, :error, {:list, nil}], {:tuple, [:atom, :error, [:number]]}}}
    ],
    compile_opts: [:debug_info],
    definitions: [
      {{:terminate, 2}, :def,
       [line: 5, file: {"lib/gen_server.ex", 840}, context: GenServer],
       [
         {[line: 5, file: {"lib/gen_server.ex", 840}, context: GenServer],
          [
            {:_reason, [version: 0, line: 840, counter: {Examples.Stack, 7}],
             GenServer},
            {:_state, [version: 1, line: 840, counter: {Examples.Stack, 7}],
             GenServer}
          ], [], :ok}
       ]},
      {{:start_link, 1}, :def, [line: 12],
       [
         {[line: 12], [{:default, [version: 0, line: 12], nil}],
          [
            {{:., [line: 12], [:erlang, :is_list]}, [line: 12],
             [{:default, [version: 0, line: 12], nil}]}
          ],
          {{:., [line: 13], [GenServer, :start_link]}, [line: 13],
           [Examples.Stack, {:default, [version: 0, line: 13], nil}]}}
       ]},
      {{:push, 2}, :def, [line: 20],
       [
         {[line: 20],
          [
            {:pid, [version: 0, line: 20], nil},
            {:element, [version: 1, line: 20], nil}
          ], [],
          {{:., [line: 21], [GenServer, :call]}, [line: 21],
           [
             {:pid, [version: 0, line: 21], nil},
             {:push, {:element, [version: 1, line: 21], nil}}
           ]}}
       ]},
      {{:pop, 1}, :def, [line: 24],
       [
         {[line: 24], [{:pid, [version: 0, line: 24], nil}], [],
          {{:., [line: 25], [GenServer, :call]}, [line: 25],
           [{:pid, [version: 0, line: 25], nil}, {:{}, [line: 25], [:pop]}]}}
       ]},
      {{:init, 1}, :def, [line: 31],
       [
         {[line: 31], [{:stack, [version: 0, line: 31], nil}], [],
          {:ok, {:stack, [version: 0, line: 32], nil}}}
       ]},
      {{:handle_info, 2}, :def,
       [line: 5, file: {"lib/gen_server.ex", 795}, context: GenServer],
       [
         {[line: 5, file: {"lib/gen_server.ex", 795}, context: GenServer],
          [
            {:msg, [version: 0, line: 795, counter: {Examples.Stack, 7}],
             GenServer},
            {:state, [version: 1, line: 795, counter: {Examples.Stack, 7}],
             GenServer}
          ], [],
          {:__block__, [line: 0],
           [
             {:=, [line: 796],
              [
                {:proc, [version: 3, line: 796, counter: {Examples.Stack, 7}],
                 GenServer},
                {:case, [line: 797],
                 [
                   {{:., [line: 797], [Process, :info]}, [line: 797],
                    [
                      {{:.,
                        [line: 797, context: GenServer, imports: [{0, Kernel}]],
                        [:erlang, :self]},
                       [line: 797, context: GenServer, imports: [{0, Kernel}]],
                       []},
                      :registered_name
                    ]},
                   [
                     do: [
                       {:->, [line: 798],
                        [
                          [{{:_, [line: 798], GenServer}, []}],
                          {{:.,
                            [
                              line: 798,
                              context: GenServer,
                              imports: [{0, Kernel}]
                            ], [:erlang, :self]},
                           [
                             line: 798,
                             context: GenServer,
                             imports: [{0, Kernel}]
                           ], []}
                        ]},
                       {:->, [line: 799],
                        [
                          [
                            {{:_, [line: 799], GenServer},
                             {:name,
                              [
                                version: 2,
                                line: 799,
                                counter: {Examples.Stack, ...}
                              ], GenServer}}
                          ],
                          {:name,
                           [version: 2, line: 799, counter: {Examples.Stack, 7}],
                           GenServer}
                        ]}
                     ]
                   ]
                 ]}
              ]},
             {{:., [line: 802], [:logger, :error]}, [line: 802],
              [
                {:%{}, [line: 803],
                 [
                   label: {GenServer, :no_handle_info},
                   report: {:%{}, [line: 805],
                    [
                      module: Examples.Stack,
                      message: {:msg,
                       [version: 0, line: 807, counter: {Examples.Stack, 7}],
                       GenServer},
                      name: {:proc,
                       [version: 3, line: 808, counter: {Examples.Stack, 7}],
                       GenServer}
                    ]}
                 ]},
                {:%{}, [line: 811],
                 [
                   domain: [:otp, :elixir],
                   error_logger: {:%{}, [line: 813], [tag: :error_msg]},
                   report_cb: {:&, [line: 814],
                    [
                      {:/, [],
                       [{{:., [], [GenServer, :format_report]}, [], []}, 1]}
                    ]}
                 ]}
              ]},
             {:noreply,
              {:state, [version: 1, line: 818, counter: {Examples.Stack, 7}],
               GenServer}}
           ]}}
       ]},
      {{:handle_cast, 2}, :def,
       [line: 5, file: {"lib/gen_server.ex", 822}, context: GenServer],
       [
         {[line: 5, file: {"lib/gen_server.ex", 822}, context: GenServer],
          [
            {:msg, [version: 0, line: 822, counter: {Examples.Stack, 7}],
             GenServer},
            {:state, [version: 1, line: 822, counter: {Examples.Stack, 7}],
             GenServer}
          ], [],
          {:__block__, [line: 0],
           [
             {:=, [line: 823],
              [
                {:proc, [version: 3, line: 823, counter: {Examples.Stack, 7}],
                 GenServer},
                {:case, [line: 824],
                 [
                   {{:., [line: 824], [Process, :info]}, [line: 824],
                    [
                      {{:.,
                        [line: 824, context: GenServer, imports: [{0, Kernel}]],
                        [:erlang, :self]},
                       [line: 824, context: GenServer, imports: [{0, Kernel}]],
                       []},
                      :registered_name
                    ]},
                   [
                     do: [
                       {:->, [line: 825],
                        [
                          [{{:_, [line: 825], GenServer}, []}],
                          {{:.,
                            [line: 825, context: GenServer, imports: [{0, ...}]],
                            [:erlang, :self]},
                           [
                             line: 825,
                             context: GenServer,
                             imports: [{0, Kernel}]
                           ], []}
                        ]},
                       {:->, [line: 826],
                        [
                          [
                            {{:_, [line: 826], GenServer},
                             {:name, [version: 2, line: 826, counter: {...}],
                              GenServer}}
                          ],
                          {:name,
                           [version: 2, line: 826, counter: {Examples.Stack, 7}],
                           GenServer}
                        ]}
                     ]
                   ]
                 ]}
              ]},
             {:case, [line: 830],
              [
                {{:., [line: 830], [:erlang, :phash2]}, [line: 830], [1, 1]},
                [
                  do: [
                    {:->, [line: 831],
                     [
                       [0],
                       {{:., [line: 832], [:erlang, :error]}, [line: 832],
                        [
                          {{:., [line: 832], [RuntimeError, :exception]},
                           [line: 832],
                           [{:<<>>, [alignment: 0, line: 832], [{...}, ...]}]},
                          :none,
                          [error_info: {:%{}, [line: 832], [module: Exception]}]
                        ]}
                     ]},
                    {:->, [line: 834],
                     [
                       [1],
                       {:{}, [line: 835],
                        [
                          :stop,
                          {:bad_cast,
                           {:msg, [version: 0, line: 835, ...], GenServer}},
                          {:state, [version: 1, line: 835, counter: {...}],
                           GenServer}
                        ]}
                     ]}
                  ]
                ]
              ]}
           ]}}
       ]},
      {{:handle_call, 3}, :def, [line: 50],
       [
         {[line: 50],
          [
            {:req, [version: 0, line: 50], nil},
            {:_from, [version: 1, line: 50], nil},
            []
          ], [],
          {:case, [line: 51],
           [
             {:req, [version: 0, line: 51], nil},
             [
               do: [
                 {:->, [line: 52],
                  [
                    [push: {:element, [version: 2, line: 52], nil}],
                    {:{}, [line: 52],
                     [
                       :reply,
                       "pushed",
                       [{:element, [version: 2, line: 52], nil}]
                     ]}
                  ]},
                 {:->, [line: 53],
                  [
                    [{:{}, [line: 53], [{:_, [line: 53], nil}]}],
                    {:{}, [line: 53], [:reply, "error", []]}
                  ]}
               ]
             ]
           ]}}
       ]},
      {{:code_change, 3}, :def,
       [line: 5, file: {"lib/gen_server.ex", 845}, context: GenServer],
       [
         {[line: 5, file: {"lib/gen_server.ex", 845}, context: GenServer],
          [
            {:_old, [version: 0, line: 845, counter: {Examples.Stack, 7}],
             GenServer},
            {:state, [version: 1, line: 845, counter: {Examples.Stack, 7}],
             GenServer},
            {:_extra, [version: 2, line: 845, counter: {Examples.Stack, 7}],
             GenServer}
          ], [],
          {:ok,
           {:state, [version: 1, line: 846, counter: {Examples.Stack, 7}],
            GenServer}}}
       ]},
      {{:child_spec, 1}, :def,
       [line: 5, file: {"lib/gen_server.ex", 762}, context: GenServer],
       [
         {[line: 5, file: {"lib/gen_server.ex", 762}, context: GenServer],
          [
            {:init_arg, [version: 0, line: 762, counter: {Examples.Stack, 7}],
             GenServer}
          ], [],
          {:__block__, [line: 0],
           [
             {:=, [line: 763],
              [
                {:default,
                 [version: 1, line: 763, counter: {Examples.Stack, 7}],
                 GenServer},
                {:%{}, [line: 763],
                 [
                   id: Examples.Stack,
                   start: {:{}, [line: 765],
                    [
                      Examples.Stack,
                      :start_link,
                      [
                        {:init_arg, [version: 0, line: 765, counter: {...}],
                         GenServer}
                      ]
                    ]}
                 ]}
              ]},
             {{:., [line: 768], [Supervisor, :child_spec]}, [line: 768],
              [
                {:default,
                 [version: 1, line: 768, counter: {Examples.Stack, 7}],
                 GenServer},
                []
              ]}
           ]}}
       ]}
    ],
    deprecated: [],
    file: "c:/Users/farad/Documents/ElixirPlayground/ElixirST/lib/elixirst/examples/stack.ex",
    is_behaviour: false,
    line: 1,
    module: Examples.Stack,
    relative_file: "lib/elixirst/examples/stack.ex",
    struct: nil,
    unreachable: []
  }
]
