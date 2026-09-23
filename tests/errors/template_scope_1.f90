! C1601 of the Fortran 2028 working draft (J3/26-007r1, 16.1.1):
!
!     A template-construct shall only appear in the specification-part of a
!     main program, module, or TEMPLATE construct.
!
! LFortran used to accept a template construct in any specification part, in
! particular inside an ordinary subprogram. A submodule is deliberately not in
! the list above, so a template in a submodule is rejected as well.
!
! See integration_tests/template_simple_01.f90 for a template in a module,
! integration_tests/template_program_01.f90 for one in a main program and
! integration_tests/template_05.f90 for a template nested in another template.

module template_scope_1_mod
    implicit none

    ! A requirement is not one of the three permitted contexts.
    requirement r {t}
        deferred type :: t
        template req_tmpl(u)  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
    end requirement

contains

    subroutine s()
        template sub_tmpl(u)  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
    end subroutine

    integer function f()
        template func_tmpl(u)  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
            deferred type :: u
        end template
        f = 1
    end function

end module

module template_scope_1_submod_mod
    implicit none
    interface
        module subroutine g()
        end subroutine
    end interface
end module

submodule (template_scope_1_submod_mod) template_scope_1_submod
    implicit none

    ! A submodule is not a module for the purposes of C1601.
    template submod_tmpl(u)  ! {Error} a template can only be declared in the specification part of a main program, a module or another template
        deferred type :: u
    end template

contains

    module subroutine g()
    end subroutine

end submodule
