program arrays_132
    implicit none
    call s(3_8, [3_8, 1_8, 2_8])
contains
    subroutine s(n, v)
        integer(8), intent(in) :: n
        integer(8), intent(in) :: v(n)
        if (any(minloc(v) /= [2])) error stop
    end subroutine s
end program arrays_132
