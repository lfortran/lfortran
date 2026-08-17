subroutine dummy_external(x)
    implicit none
    integer, intent(inout) :: x
    x = x + 1
end subroutine
