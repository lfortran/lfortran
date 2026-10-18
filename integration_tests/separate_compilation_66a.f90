module separate_compilation_66a_funcs
implicit none
abstract interface
    integer function ifunc(x)
        integer, intent(in) :: x
    end function
end interface
contains
    integer function add_one(x)
        integer, intent(in) :: x
        add_one = x + 1
    end function
end module

module separate_compilation_66a
use separate_compilation_66a_funcs
implicit none
type :: pf_t
    procedure(ifunc), pointer, nopass :: f => null()
end type
contains
    integer function call_local(x)
        integer, intent(in) :: x
        procedure(ifunc), pointer :: p => add_one
        call_local = p(x)
    end function

    integer function call_saved(x)
        integer, intent(in) :: x
        type(pf_t), save :: s = pf_t(add_one)
        call_saved = s%f(x)
    end function
end module
