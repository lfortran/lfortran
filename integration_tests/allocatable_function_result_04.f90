! A function with a polymorphic pointer result in a specification
! expression. Like an allocatable result, it is returned through an
! argument, so the expression is evaluated in a helper function.
module allocatable_function_result_04_m
    implicit none
    type :: t
        integer :: n = 0
    end type
contains
    pure function none() result(r)
        class(t), pointer :: r
        r => null()
    end function

    pure function length(x) result(k)
        class(t), pointer, intent(in) :: x
        integer :: k
        if (associated(x)) then
            k = x%n
        else
            k = 4
        end if
    end function

    subroutine check()
        real :: a(length(none()))
        character(len=length(none())) :: c
        if (size(a) /= 4) error stop 1
        if (len(c) /= 4) error stop 2
    end subroutine
end module

program allocatable_function_result_04
    use allocatable_function_result_04_m
    implicit none
    call check()
    print *, "ok"
end program
