! External procedures called through implicit interfaces from
! string_arg_hidden_length_02.f90, compiled separately.
subroutine ext_scalars(a, n, b, c)
    implicit none
    character(len=*), intent(inout) :: a
    integer, intent(in) :: n
    character(len=3), intent(in) :: b
    character, intent(out) :: c
    if (len(a) /= n) error stop 101
    if (b /= 'xyz') error stop 102
    a(1:1) = 'J'
    c = b(2:2)
end subroutine

subroutine ext_array(x, n)
    implicit none
    integer, intent(in) :: n
    character(len=*), intent(inout) :: x(n)
    integer :: i
    if (len(x) /= 2) error stop 103
    do i = 1, n
        x(i) = x(i)(2:2) // x(i)(1:1)
    end do
end subroutine

integer function ext_len(s)
    implicit none
    character(len=*), intent(in) :: s
    ext_len = len(s)
end function

subroutine ext_apply(f, s, r)
    implicit none
    external f
    character(len=*), intent(in) :: s
    integer, intent(out) :: r
    call f(s, r)
end subroutine
