subroutine mu03_add(k, n)
    integer, intent(inout) :: k
    integer, intent(in) :: n
    k = k + n
end subroutine

integer function mu03_twice(n)
    integer, intent(in) :: n
    mu03_twice = 2*n
end function

subroutine mangle_underscore_03_m_x(k)
    integer, intent(inout) :: k
    k = k + 10
end subroutine

subroutine mangle_underscore_03_m_t(k)
    integer, intent(inout) :: k
    k = k + 1000
end subroutine
