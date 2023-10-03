subroutine swap(x, y)
    integer, intent(inout) :: x, y
    integer :: tmp
    tmp = x
    x = y
    y = tmp
end subroutine reverse

program name
end program name
