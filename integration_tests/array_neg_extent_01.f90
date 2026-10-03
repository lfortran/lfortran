function f(A) result(res)
    real, intent(in) :: A(:)
    real :: res(size(A) - 4)
    res = 0
end function

program array_neg_extent_01
    implicit none
    interface
        function f(A) result(res)
            real, intent(in) :: A(:)
            real :: res(size(A) - 4)
        end function
    end interface
    real :: a3(3), a6(6)
    integer :: s

    ! size(A) - 4 = 3 - 4 = -1 => zero-size array
    s = size(f(a3))
    print *, s
    if (s /= 0) error stop

    ! size(A) - 4 = 6 - 4 = 2 => two-element array
    s = size(f(a6))
    print *, s
    if (s /= 2) error stop
end program
