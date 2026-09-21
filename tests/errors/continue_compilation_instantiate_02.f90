! Erroneous instantiation arguments under --continue-compilation.
!
! Each statement below names something that cannot be an instantiation
! argument for the corresponding template parameter. The symbol table visitor
! used to dereference the resolved symbol without checking it, so an
! undeclared name segfaulted and a symbol of the wrong kind tripped an
! assertion inside ASRUtils::symbol_type.
!
! tests/errors/continue_compilation_instantiate_01.f90 covers erroneous
! template names and argument counts; this file covers the arguments
! themselves.

module continue_compilation_instantiate_02_mod
    implicit none

    template type_tmpl(t)
        deferred type :: t
    contains
        subroutine s(x)
            type(t), intent(in) :: x
        end subroutine
    end template

    template const_tmpl(t, n)
        deferred type :: t
        integer :: n
    contains
        subroutine sn(x)
            type(t), intent(in) :: x
            integer :: i
            do i = 1, n
                print *, i
            end do
        end subroutine
    end template

contains

    subroutine helper()
    end subroutine

end module continue_compilation_instantiate_02_mod

program continue_compilation_instantiate_02
    use continue_compilation_instantiate_02_mod
    implicit none

    ! Undeclared name as the argument for a deferred type
    instantiate type_tmpl {no_such_type}, only: s1 => s

    ! A subroutine is not a type
    instantiate type_tmpl {helper}, only: s2 => s

    ! Undeclared name as the argument for a non-type parameter
    instantiate const_tmpl {integer, no_such_n}, only: s3 => sn
end program continue_compilation_instantiate_02
