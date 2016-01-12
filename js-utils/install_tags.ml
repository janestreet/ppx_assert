let package_name = "ppx_assert"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_assert", None)
    ; ("built_lib_ppx_assert_lib", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
