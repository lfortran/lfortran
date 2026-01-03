! Check that lcompilers_get_i() returns mutated value of i not just hard coded value of 4
module arrays_51_mod
    IMPLICIT NONE
    integer :: i = 4
    integer :: arr_mod(4,6)
    contains
    subroutine sub (arr)
        integer , INTENT(OUT):: arr(4,i)
        print *,size(arr)
        if(size(arr) /= 24) error stop
    end subroutine sub
end module arrays_51_mod


program arrays_51
    use arrays_51_mod
    i = 6
    call sub(arr_mod)
end program arrays_51
