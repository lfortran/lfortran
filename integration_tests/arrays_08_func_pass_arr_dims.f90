program arrays_08_func
implicit none
integer :: x(10), y(10), i
logical :: r
do i = 1, size(x)
    x(i) = i
end do
call copy_from_to(size(x, dim=1), x, size(y, dim=1), y)
r = verify(size(x, dim=1), x, size(y, dim=1), y)
print *, r
if (.not. r) error stop

contains

    subroutine copy_from_to(na1, a, nb1, b)
    integer, intent(in) :: na1, nb1
    integer, intent(in) :: a(na1)
    integer, intent(out) :: b(nb1)
    integer :: i
    do i = 1, size(a)
        b(i) = a(i)
    end do
    end subroutine

    logical function verify(na1, a, nb1, b) result(r)
    integer, intent(in) :: na1, nb1
    integer, intent(in) :: a(na1), b(nb1)
    integer :: i
    r = .true.
    do i = 1, size(a)
        r = r .and. (a(i) .eq. b(i))
    end do
    end function

end
