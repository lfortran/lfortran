! The Fortran 2028 working draft (J3/26-007r1) contradicts itself about where a
! REQUIREMENT construct may appear:
!
!     R1605 template-declaration  is  template-specification
!                                 or  deferred-arg-decl-stmt
!                                 or  requirement-construct
!                                 or  template-construct
!
!     C1636 A requirement-construct shall only appear in the specification-part
!           of a main program or module.
!
! R1605 permits a requirement construct inside a TEMPLATE construct, C1636
! forbids it. This is a drafting defect, not a settled rule; LFortran follows
! C1636 because rejecting is reversible, while accepting code the standard may
! forbid creates a compatibility burden if J3 resolves it the other way.
!
! A submodule, a subprogram and another requirement are not in C1636's list
! either, so a requirement is rejected in those as well.
!
! See integration_tests/template_simple_01.f90 for a requirement in a module and
! integration_tests/template_03b.f90 for one in a main program.

module requirement_scope_1_mod
    implicit none

    ! Permitted: the specification part of a module.
    requirement ok_r {t}
        deferred type :: t
    end requirement

    ! R1605 would allow this, C1636 does not.
    template tmpl(t)
        deferred type :: t
        requirement tmpl_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

    ! A requirement is not a main program or a module either.
    requirement outer_r {t}
        deferred type :: t
        requirement inner_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    end requirement

contains

    subroutine sub()
        requirement sub_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
    end subroutine

    integer function func()
        requirement func_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
            deferred type :: u
        end requirement
        func = 1
    end function

end module

module requirement_scope_1_submod_mod
    implicit none
    interface
        module subroutine g()
        end subroutine
    end interface
end module

submodule (requirement_scope_1_submod_mod) requirement_scope_1_submod
    implicit none

    ! A submodule is not a module for the purposes of C1636.
    requirement submod_r {u}  ! {Error} a requirement can only be declared in the specification part of a main program or a module
        deferred type :: u
    end requirement

contains

    module subroutine g()
    end subroutine

end submodule
