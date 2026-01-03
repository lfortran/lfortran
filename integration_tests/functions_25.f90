module f
    implicit none
    integer, parameter :: sp = selected_real_kind(6)
    contains

    module function diag_rsp_k(v, k) result(res)
        real(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(sp) :: res(size(v) + abs(k), size(v) + abs(k))
        integer :: i, sz

        sz = size(v)
        res = 0
        if (k > 0) then
        do i = 1, sz
            res(i, k + i) = v(i)
        end do
        else if (k < 0) then
        do i = 1, sz
            res(i + abs(k), i) = v(i)
        end do
        else
        do i = 1, sz
            res(i, i) = v(i)
        end do
        end if
    end function diag_rsp_k
end module f

program functions_25
    use f
    implicit none
    integer :: i
    integer, parameter :: n = 4
    real(sp) :: a(n, n), c(n, n)

    c = diag_rsp_k([(1._sp, i=1, n - 1)], 1)
    c = transpose(c)
    a = diag_rsp_k([(1._sp, i = 1, n - 1)], -1)
    print *, all(a == c)
    if (.not. all(a == c)) error stop
end program
