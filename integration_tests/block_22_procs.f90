module block_22_m
    implicit none
contains
    integer function q(k)
        integer, intent(in) :: k
        q = 1000*k
    end function q
end module block_22_m

integer function q(k)
    implicit none
    integer, intent(in) :: k
    q = 3*k
end function q

subroutine s(y)
    implicit none
    integer, intent(out) :: y
    y = 30
end subroutine s
