subroutine mu03_add(k, n)
    integer, intent(inout) :: k
    integer, intent(in) :: n
    k = k + n
end subroutine

integer function mu03_twice(n)
    integer, intent(in) :: n
    mu03_twice = 2*n
end function
