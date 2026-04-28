program array_bound_7
    implicit none
    integer :: lb1
    real :: a(-2:2)
    lb1 = -2
    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    call outer_proc(a)
contains
    subroutine outer_proc(arr)
        real :: arr(lb1:)
        print *, "outer lbound:", lbound(arr,1), " ubound:", ubound(arr,1)
        if (lbound(arr,1) /= -2) error stop "wrong lbound in outer"
        if (ubound(arr,1) /=  2) error stop "wrong ubound in outer"
        call inner_proc(arr)
    end subroutine outer_proc

    subroutine inner_proc(arr)
        real :: arr(lb1:)
        print *, "inner lbound:", lbound(arr,1), " ubound:", ubound(arr,1)
        if (lbound(arr,1) /= -2) error stop "wrong lbound in inner"
        if (ubound(arr,1) /=  2) error stop "wrong ubound in inner"
    end subroutine inner_proc
end program array_bound_7
