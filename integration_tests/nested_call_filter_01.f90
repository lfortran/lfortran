module nested_call_filter_mod
contains
    subroutine modproc(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine modproc
end module nested_call_filter_mod

program nested_call_filter_01
    use nested_call_filter_mod, only: modproc
    implicit none
    call outer()
contains
    subroutine outer()
        integer :: a
        a = 1
        call inner()
        call modproc(a)
    contains
        subroutine inner()
            a = a + 1
        end subroutine inner
    end subroutine outer
end program nested_call_filter_01
