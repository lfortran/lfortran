subroutine sub(a, b, i)
    real(8) :: a, b
    real(8), save :: c
    integer :: i
    if (i == 1) then
        print *, c
        if (abs(c-2.4D0) > 1.0D-12) error stop
        return
    end if
    c = a - b
end subroutine

program save_04
    call sub(3.4D0, 1.0D0, 0)
    call sub(3.4D0, 1.0D0, 1)
end program