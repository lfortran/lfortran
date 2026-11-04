! Dummy arguments of elemental procedures (F2018 C15100, C15103).
! The array case is in elemental_dummy_array.f90, as ASR verify would also report it here.
module cc_elem_dummies_m
    implicit none
    abstract interface
        pure integer function cc_elem_dummies_iface(i)
            integer, intent(in) :: i
        end function
    end interface
contains
    elemental subroutine cc_elem_dummies_pointer(x)
        integer, pointer, intent(in) :: x  ! {Error} dummy argument 'x' of elemental procedure 'cc_elem_dummies_pointer' cannot be a pointer
    end subroutine

    elemental subroutine cc_elem_dummies_coarray(x)
        integer, intent(in) :: x[*]  ! {Error} dummy argument 'x' of elemental procedure 'cc_elem_dummies_coarray' cannot be a coarray
    end subroutine

    elemental subroutine cc_elem_dummies_procedure(g, h)
        interface
            pure integer function g(i)  ! {Error} dummy argument 'g' of elemental procedure 'cc_elem_dummies_procedure' cannot be a procedure
                integer, intent(in) :: i
            end function
        end interface
        procedure(cc_elem_dummies_iface), pointer, intent(in) :: h  ! {Error} dummy argument 'h' of elemental procedure 'cc_elem_dummies_procedure' cannot be a procedure
    end subroutine

    elemental subroutine cc_elem_dummies_alt_return(x, *)  ! {Error} elemental procedure 'cc_elem_dummies_alt_return' cannot have an alternate return dummy argument '*'
        integer, intent(in) :: x
    end subroutine
end module
