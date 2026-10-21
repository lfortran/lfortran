program legacy_array_sections_30
    implicit none
    real :: x(3)
    character(len=3) :: c(3)
    x = 1.0
    call s(x)
    print *, x
    if (abs(x(1) - 1.0) > 1e-6) error stop
    if (abs(x(2) - 4.0) > 1e-6) error stop
    if (abs(x(3) - 1.0) > 1e-6) error stop
    c = ["aaa", "bbb", "ccc"]
    call t(c, 3)
end program

subroutine s(x)
    implicit none
    real :: x
    dimension x(1)
    if (size(x) /= 1) error stop
    if (ubound(x, 1) /= 1) error stop
    x(2) = 4.0
end subroutine

subroutine t(c, n)
    implicit none
    integer, intent(in) :: n
    character(len=3), intent(in) :: c(1)
    print *, c(n)(2:3), c(n)
    if (c(n)(2:3) /= "cc") error stop
    if (c(n) /= "ccc") error stop
end subroutine
