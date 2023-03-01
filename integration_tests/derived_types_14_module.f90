module derived_types_14_module
    !> Possible kinds of TOML values in key-value pairs
    implicit none
    private
    type :: enum_type

       !> Invalid type
       integer :: invalid = 100

       !> String type
       integer :: string = 101

       !> Boolean type
       integer :: boolean = 102

       !> Integer type
       integer :: int = 103

       !> Float type
       integer :: float = 104

       !> Datetime type
       integer :: datetime = 105

    end type enum_type

    !> Actual enumerator with TOML value types
    type(enum_type), public, parameter :: toml_type = enum_type()

 end module
