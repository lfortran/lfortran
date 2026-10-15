module template_local_type_03_funcs
    implicit none
    integer :: hits = 0
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function

    subroutine bump()
        hits = hits + 1
    end subroutine
end module

! The interface of a procedure pointer declared in a procedure of a template
! can be an entity of an enclosing procedure of the template: an internal
! procedure or an abstract interface of the procedure declaring a derived
! type with a procedure pointer component, or an internal procedure of the
! host of the internal procedure declaring the type or a procedure pointer.
! Each instantiated procedure uses its own instance of that interface.
module template_local_type_03_m
    use template_local_type_03_funcs, only: hits, twice, bump
    implicit none

    template tm {t}
        deferred type :: t
    contains
        integer function own_internal(x) result(r)
            integer, intent(in) :: x
            type :: tt
                procedure(later), pointer, nopass :: pf => null()
            end type
            type(tt) :: a
            a%pf => twice
            r = a%pf(x)
            a%pf => later
            r = r + a%pf(x)
        contains
            integer function later(y)
                integer, intent(in) :: y
                later = 100*y
            end function
        end function

        integer function own_abstract(x) result(r)
            integer, intent(in) :: x
            abstract interface
                integer function iface(y)
                    integer, intent(in) :: y
                end function
            end interface
            type :: tt
                procedure(iface), pointer, nopass :: pf => null()
            end type
            type(tt) :: a
            a%pf => twice
            r = a%pf(x)
        end function

        subroutine own_nested()
            call inner()
        contains
            subroutine inner()
                type :: tt
                    procedure(later), pointer, nopass :: pf => null()
                end type
                type(tt) :: a
                procedure(later), pointer :: pf
                pf => bump
                call pf()
                pf => later
                call pf()
                a%pf => bump
                call a%pf()
                a%pf => later
                call a%pf()
            end subroutine

            subroutine later()
                hits = hits + 10
            end subroutine
        end subroutine
    end template
end module

program template_local_type_03
    use template_local_type_03_funcs, only: hits
    use template_local_type_03_m
    implicit none
    instantiate tm {integer}, only: own_internal => own_internal, &
        own_abstract => own_abstract, own_nested => own_nested
    print *, own_internal(3), own_abstract(4)
    if (own_internal(3) /= 306) error stop
    if (own_abstract(4) /= 8) error stop
    call own_nested()
    print *, hits
    if (hits /= 22) error stop
end program
