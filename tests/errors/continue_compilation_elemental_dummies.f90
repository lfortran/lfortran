! Dummy arguments of elemental procedures (F2018 C15100, C15103).
module cc_elem_dummies_m
    implicit none
    abstract interface
        pure integer function cc_elem_dummies_iface(i)
            integer, intent(in) :: i
        end function
    end interface
contains
    elemental integer function cc_elem_dummies_array(x)
        integer, intent(in) :: x(3)  ! {Error} dummy argument 'x' of elemental procedure 'cc_elem_dummies_array' must be scalar
        cc_elem_dummies_array = 1
    end function

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
