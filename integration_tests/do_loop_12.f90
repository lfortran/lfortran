program do_loop_12
    ! LLVM 7 and 8 miscompile this loop nest under --fast when tuning for an
    ! x86-64 CPU with a loop buffer: values(1:n) is compared past n = 1.
    implicit none
    integer :: calls = 0, wrappers = 0, n, i
    integer :: values(3)
    do n = 0, 3
        calls = 0
        wrappers = 0
        values = -1
        values(1:n) = codes(trim(sized_text(n)))
        if (calls /= 1 .or. wrappers /= 1) error stop 1
        if (any(values(1:n) /= [(120 + i, i = 1, n)])) error stop 2
        if (any(values(n+1:) /= -1)) error stop 3
    end do
    print *, values
contains
    function sized_text(length) result(r)
        integer, intent(in) :: length
        character(5) :: r
        calls = calls + 1
        r = repeat('x', length)
    end function
    function codes(value) result(r)
        character(*), intent(in) :: value
        integer :: r(len(value)), i
        wrappers = wrappers + 1
        do i = 1, len(value)
            r(i) = iachar(value(i:i)) + i
        end do
    end function
end program
