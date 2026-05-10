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
    public :: foo
contains
    subroutine foo()
        type(sep_comp_ext_sym_m1) :: f
        f%var = 1
    end subroutine
end module
