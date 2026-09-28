! In a submodule of a module compiled separately that declared the symbol for
! `l%u`, a structure constructor `l%u(1)` is passed for a dummy argument of
! another type: the type is shown by its name, `u`.
submodule (nssep_parent) namespace_separate_submodule
    implicit none
contains
    module subroutine run()
        call l%take(l%u(1))
    end subroutine
end submodule
