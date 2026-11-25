! Arrays and subroutines in the Lean backend: arrays are values, and a
! subroutine returns its intent(out) and intent(inout) arguments.
module lean_arrays_01_mod
implicit none
contains

    ! p = c(1) + c(2) x + ... + c(n) x**(n-1), by Horner's rule
    subroutine horner(n, c, x, p)
    integer, intent(in) :: n
    real(8), intent(in) :: c(n)
    real(8), intent(in) :: x
    real(8), intent(out) :: p
    integer :: i
    p = c(n)
    do i = 1, n - 1
        p = p*x + c(n - i)
    end do
    end subroutine

    subroutine scale(a, s)
    real(8), intent(inout) :: a(:)
    real(8), intent(in) :: s
    integer :: i
    do i = 1, size(a)
        a(i) = s*a(i)
    end do
    end subroutine

    subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: t
    t = a
    a = b
    b = t
    end subroutine

    ! b(i, j) = a(j, i) for an explicit-shape n x m array a
    subroutine transpose_into(n, m, a, b)
    integer, intent(in) :: n, m
    integer, intent(in) :: a(n, m)
    integer, intent(out) :: b(m, n)
    integer :: i, j
    do j = 1, m
        do i = 1, n
            b(j, i) = a(i, j)
        end do
    end do
    end subroutine

    ! The index of the first negative element of v(0:), or -1.
    subroutine first_negative(v, k, found)
    real(8), intent(in) :: v(0:)
    integer, intent(out) :: k
    logical, intent(out) :: found
    integer :: i
    found = .false.
    k = -1
    do i = lbound(v, 1), ubound(v, 1)
        if (v(i) < 0) then
            k = i
            found = .true.
            return
        end if
    end do
    end subroutine

    ! Calls another subroutine, and assigns whole arrays.
    subroutine double_and_sort2(a)
    real(8), intent(inout) :: a(2)
    real(8) :: t(2)
    integer :: i1, i2
    t = a
    call scale(t, 2.0d0)
    a = t
    if (a(1) > a(2)) then
        i1 = 1
        i2 = 2
        call swap(i1, i2)
        t(1) = a(i1)
        t(2) = a(i2)
        a = t
    end if
    end subroutine

    real(8) function total(m)
    real(8), intent(in) :: m(:,:)
    integer :: i, j
    total = 0
    do j = lbound(m, 2), ubound(m, 2)
        do i = 1, size(m, 1)
            total = total + m(i, j)
        end do
    end do
    end function

    ! A do-loop variable that is also the function result or an intent(out)/
    ! intent(inout) dummy must keep its post-loop value.
    integer function loop_result(n) result(i)
    integer, intent(in) :: n
    do i = 1, n
    end do
    end function

    subroutine loop_out(n, i)
    integer, intent(in) :: n
    integer, intent(out) :: i
    do i = 1, n
    end do
    end subroutine

    subroutine loop_inout(n, i)
    integer, intent(in) :: n
    integer, intent(inout) :: i
    do i = 1, n
    end do
    end subroutine

end module

program lean_arrays_01
use lean_arrays_01_mod
implicit none
real(8) :: c(3), p, v(0:2), m(2, 3), w(2)
integer :: i, j, a, b, x(2, 3), y(3, 2), k, z(5:3)
logical :: found
c(1) = 1
c(2) = 2
c(3) = 3
call horner(3, c, 2.0d0, p)
if (p /= 17) error stop
v(0) = 1
v(1) = 2
v(2) = 3
call scale(v, 2.0d0)
if (v(0) /= 2 .or. v(1) /= 4 .or. v(2) /= 6) error stop
if (lbound(v, 1) /= 0 .or. ubound(v, 1) /= 2) error stop
a = 1
b = 2
call swap(a, b)
if (a /= 2 .or. b /= 1) error stop
do j = 1, 3
    do i = 1, 2
        m(i, j) = i + 10*j
        x(i, j) = i + 10*j
    end do
end do
if (total(m) /= 129) error stop
call transpose_into(2, 3, x, y)
if (y(3, 1) /= 31 .or. y(1, 2) /= 12 .or. size(y, 1) /= 3) error stop
v(1) = -1
call first_negative(v, k, found)
if (.not. found .or. k /= 1) error stop
v = 5
call first_negative(v, k, found)
if (found .or. k /= -1) error stop
w(1) = 3
w(2) = 1
call double_and_sort2(w)
if (w(1) /= 2 .or. w(2) /= 6) error stop
if (loop_result(5) /= 6) error stop
if (loop_result(0) /= 1) error stop
call loop_out(5, k)
if (k /= 6) error stop
k = 99
call loop_inout(5, k)
if (k /= 6) error stop
k = 99
call loop_inout(0, k)
if (k /= 1) error stop
k = -5
if (abs(k) /= 5) error stop
if (lbound(z, 1) /= 1 .or. ubound(z, 1) /= 0 .or. size(z) /= 0) error stop
print *, p, v(2), a, total(m), y(3, 1), k
end program
