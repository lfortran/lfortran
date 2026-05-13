module sep_comp_ext_sym_m1
    implicit none
end module

module sep_comp_ext_sym_m2
    implicit none
    type :: sep_comp_ext_sym_m1
        integer :: var
    end type
end module

module sep_comp_ext_sym_use_m1
    use sep_comp_ext_sym_m1
    implicit none
    private
    public :: bar
contains
    subroutine bar()
    end subroutine
end module

module sep_comp_ext_sym_use_m2
    use sep_comp_ext_sym_m2
    implicit none
    private
    public :: foo, sep_comp_ext_sym_expected
    integer, parameter :: sep_comp_ext_sym_expected = 42
contains
    integer function foo()
        type(sep_comp_ext_sym_m1) :: f
        f%var = sep_comp_ext_sym_expected
        foo = f%var
    end function
end module
