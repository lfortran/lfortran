subroutine store_first(buf)
    implicit none
    integer(1), intent(inout) :: buf(*)
    buf(1) = 7_1
end subroutine
