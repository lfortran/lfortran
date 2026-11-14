module assumed_rank_33_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
contains
    function c_first(tag) bind(c) result(n)
        character(kind=c_char, len=*), intent(in) :: tag(..)
        integer(c_int) :: n
        n = -1
        select rank (tag)
        rank (0)
            n = iachar(tag(1:1))
        end select
    end function

    integer function f_len(tag) result(n)
        character(len=*), intent(in) :: tag(..)
        n = len(tag)
        if (rank(tag) /= 0) n = -1
    end function
end module

program assumed_rank_33
    use assumed_rank_33_mod
    implicit none
    character(len=6) :: name

    name = "planet"
    if (c_first(name) /= iachar("p")) error stop 1
    if (f_len(name) /= 6) error stop 2
    print *, "OK"
end program
