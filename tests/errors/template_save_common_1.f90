! C1610 and C1611 (J3/26-007r1, 16.3): within a template or templated
! procedure, or a scoping unit nested therein, an entity that is not accessed
! by host or use association shall not have the SAVE attribute, and a COMMON or
! EQUIVALENCE statement shall not appear.
module template_save_common_1_m
    implicit none

    ! C1610, in a procedure of a template, in each spelling of SAVE
    template save_tmpl(t)
        deferred type :: t
    contains
        subroutine explicit_save(x)
            type(t), intent(in) :: x
            integer, save :: counter
            counter = counter + 1
            print *, x, counter
        end subroutine

        subroutine save_statement(x)
            type(t), intent(in) :: x
            integer :: counter
            save :: counter
            counter = counter + 1
            print *, x, counter
        end subroutine

        subroutine bare_save_statement(x)
            type(t), intent(in) :: x
            integer :: counter
            save
            counter = counter + 1
            print *, x, counter
        end subroutine

        ! An initialized local has an implicit SAVE attribute
        subroutine initialized_local(x)
            type(t), intent(in) :: x
            integer :: counter = 0
            counter = counter + 1
            print *, x, counter
        end subroutine

        ! A procedure contained in a procedure of a template is a scoping unit
        ! nested in the template
        subroutine nested_procedure(x)
            type(t), intent(in) :: x
            print *, x
            call inner()
        contains
            subroutine inner()
                integer, save :: counter
                counter = counter + 1
                print *, counter
            end subroutine
        end subroutine
    end template

    ! C1610 is not reachable in the specification part of a template itself.
    ! C1603 allows only a PARAMETER declaration there, and a named constant
    ! cannot have the SAVE attribute, so such a declaration is rejected as a
    ! C1603 violation before SAVE is ever considered. C1610 is still reachable,
    ! and is covered above and below, in a procedure of a template's CONTAINS
    ! section, in a scoping unit nested in one, and in a templated procedure.

    ! C1610, in a template nested in a template
    template outer_tmpl(t)
        deferred type :: t
        template inner_tmpl(u)
            deferred type :: u
        contains
            subroutine inner_save(y)
                type(u), intent(in) :: y
                integer, save :: counter
                counter = counter + 1
                print *, y, counter
            end subroutine
        end template
    contains
        subroutine outer_sub(x)
            type(t), intent(in) :: x
            print *, x
        end subroutine
    end template

    ! C1611, in a procedure of a template
    template storage_tmpl(t)
        deferred type :: t
    contains
        subroutine common_block(x)
            type(t), intent(in) :: x
            integer :: a
            common /blk/ a
            a = 1
            print *, x, a
        end subroutine

        subroutine equivalenced(x)
            type(t), intent(in) :: x
            integer :: a, b
            equivalence (a, b)
            a = 1
            print *, x, b
        end subroutine
    end template

contains

    ! C1610, in a templated procedure
    subroutine templated_save{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        integer, save :: counter
        counter = counter + 1
        print *, x, counter
    end subroutine

    ! C1611, in a templated procedure
    subroutine templated_common{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        integer :: a
        common /blk2/ a
        a = 1
        print *, x, a
    end subroutine

end module
