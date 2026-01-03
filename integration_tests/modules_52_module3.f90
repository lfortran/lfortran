module module_52_tomlf_error
    type :: toml_error
       integer :: stat
       character(len=:), allocatable :: message
    end type toml_error
 end module
