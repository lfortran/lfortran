! transfer of a row of a character-string array's integer mold used to
! index a descriptor pointer as if it were the descriptor.
subroutine take(c, o, m, n)
    character(len=1), intent(out) :: c(m)
    integer, intent(in) :: m, n
    integer(1), intent(in) :: o(m, n)
    integer :: i
    do i = 1, m
        c(i) = transfer(o(i, :), c(i))
    end do
end subroutine

program character_string_array_01
    implicit none
    character(len=1) :: c(2)
    integer(1) :: o(2, 1)
    o(1, 1) = iachar('a')
    o(2, 1) = iachar('b')
    call take(c, o, 2, 1)
    if (c(1) /= 'a' .or. c(2) /= 'b') error stop
end program
