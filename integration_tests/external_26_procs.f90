subroutine external_26_add(x, y, z)
    implicit none
    integer, intent(in) :: x, y
    integer, intent(out) :: z
    z = x + y
end subroutine external_26_add

integer function external_26_twice(x)
    implicit none
    integer, intent(in) :: x
    external_26_twice = 2*x
end function external_26_twice
