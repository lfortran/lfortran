module separate_compilation_57_base
implicit none
type :: parent_t
    integer :: n
end type
end module

module separate_compilation_57a
use separate_compilation_57_base
implicit none
type, extends(parent_t) :: child_t
end type
contains
    type(child_t) function create()
        create = child_t(7)
    end function
end module
