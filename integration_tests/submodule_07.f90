module mod_wrapper_submodule_07
    implicit none
    private

    public :: key_type

    type :: key_type
        integer :: value
    end type key_type
end module mod_wrapper_submodule_07


module mod_submodule_07
    use mod_wrapper_submodule_07

contains
    module subroutine sub()
        print *, "Hello World"
    end subroutine
end module mod_submodule_07

submodule(mod_submodule_07) submod_submodule_07
    use mod_wrapper_submodule_07
end submodule submod_submodule_07

program main
    use mod_submodule_07
end program
