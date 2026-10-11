subroutine s(n)
    integer(8), intent(in) :: n
    integer :: v(n)
    v = [3, 1, 2]
    v([2]) = 7 
    if (v(2) /= 7) error stop
end subroutine

program array_bound_10
    implicit none
    integer(8) :: n
    n = 3
    call s(n)
end program