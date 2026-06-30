program test_polymorphic_array_element
    implicit none
    real :: a(1)

    a(1) = 42.0
    call outer(a)
    
contains

    subroutine inner(x)
        class(*) :: x
    end subroutine inner

    subroutine outer(a)
        class(*) :: a(1)
        call inner(a(1))
    end subroutine outer

end program test_polymorphic_array_element