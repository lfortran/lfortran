module assumed_rank_34_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
contains
    function c_len(tag) bind(c) result(n)
        character(kind=c_char, len=*), intent(in) :: tag(..)
        integer(c_int) :: n
        n = len(tag)
    end function

    subroutine c_set(tag, src) bind(c)
        character(kind=c_char, len=*), intent(inout) :: tag(..)
        character(kind=c_char, len=*), intent(in) :: src(..)
        select rank (tag)
        rank (0)
            if (tag /= "hello") error stop 10
            select rank (src)
            rank (0)
                tag = src
            end select
        end select
    end subroutine
end module

program assumed_rank_34
    use assumed_rank_34_mod
    implicit none
    character(len=3) :: arr(2)
    character(len=5) :: s

    arr = ["abc", "def"]
    if (c_len(arr) /= 3) error stop 1
    s = "hello"
    call c_set(s, "XY")
    if (s /= "XY") error stop 2
    s = "hello"
    call c_set(s, "pqrstuv")
    if (s /= "pqrst") error stop 3
    print *, "OK"
end program
