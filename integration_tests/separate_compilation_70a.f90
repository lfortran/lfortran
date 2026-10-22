module separate_compilation_70a_funcs
implicit none
contains
    integer function get()
        get = 7
    end function
end module

module separate_compilation_70a
use separate_compilation_70a_funcs, only: get
implicit none
type :: t
contains
    procedure, nopass :: value => get
end type
end module
