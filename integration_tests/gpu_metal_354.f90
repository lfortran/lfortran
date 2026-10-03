! A section passed to a procedure in a FORALL or an arm of a conditional
! expression is copied into a contiguous buffer before the statement, under
! the condition that the call is evaluated. That condition may call a pure
! procedure, which is then evaluated once. A FORALL step may be a variable,
! of either sign. An inner FORALL range, or the test of a conditional
! expression, may read the index of an outer FORALL: the copy is then made
! when some index evaluates the call. A section that exists only where the
! call is evaluated is not read elsewhere.
module gpu_metal_354_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
pure integer function g(n)
    integer, intent(in) :: n
    g = n + 1
end function
end module

program gpu_metal_354
use gpu_metal_354_mod
implicit none
real :: a(3,5), v(3), q(3), s1(3,3), t1(3,3), z1(3,3), z2(3,3), z3(3,3)
real :: s3(3,3,3), u(3,3,3), y(3,3), w(3,3,3), s4(3,3)
integer :: i, j, k, l, st, sn, n(3), r, c, d
real :: expect
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
v = 0; q = 0; s1 = 0; t1 = 0; z1 = -7; z2 = -7; z3 = -7
s3 = 0; u = -1; y = 0; w = 0; s4 = 0; n = 0
st = 2; sn = -1

do concurrent (i = 1:3)
    ! A test that calls a pure function.
    v(i) = (g(i) <= 3 ? row_sum(a(i,:)) : 0.0)
    ! A test that passes a section itself.
    q(i) = (row_sum(a(i,:)) > 70 ? row_sum(a(i,2:5)) : -row_sum(a(i,1:3)))
    ! In a do while condition.
    k = 0
    do while ((g(i) <= 3 ? row_sum(a(i,:)) : 1000.0) * k < 200)
        k = k + 1
    end do
    n(i) = k
    ! A FORALL bound that calls a pure function.
    forall (j = 1:g(i)-1) s4(i,j) = row_sum(a(i,1:5:2)) * j
    ! Steps that are variables, positive and negative.
    forall (j = 1:3:st) s1(i,j) = row_sum(a(i,:)) * j
    forall (j = 3:1:sn) t1(i,j) = row_sum(a(i,1:5:2)) + j
    ! These run no times for i = 3, where a(3,1:6) does not exist.
    forall (j = i:2:st-1) z1(i,j) = row_sum(a(i,1:2*i))
    forall (j = 2:i:sn) z2(i,j) = row_sum(a(i,1:2*i))
    forall (j = 1:3:sn) z3(i,j) = row_sum(a(i,1:2*i))
    ! An inner range that reads the outer index.
    forall (j = 1:3)
        forall (k = j:3) s3(i,j,k) = row_sum(a(i,:)) * j
    end forall
    ! The inner range is empty for every j when i = 3.
    forall (j = 1:3)
        forall (k = j+i:3) u(i,j,k) = row_sum(a(i,1:2*i)) * k
    end forall
    ! A test that reads the index; not taken for any j when i = 3.
    forall (j = 1:3) y(i,j) = (j + i <= 3 ? row_sum(a(i,1:2*i)) : -1.0)
    ! An outer step that is a variable, and an inner range reading it.
    forall (j = 3:1:sn)
        forall (k = 1:j:2) w(i,j,k) = row_sum(a(i,2:5)) + j
    end forall
end do

print *, v, q, n
print *, sum(s1), sum(t1), sum(z1), sum(z2), sum(z3)
print *, sum(s3), sum(u), sum(y), sum(w), sum(s4)
if (abs(v(1) - 65) > 1e-3 .or. abs(v(2) - 115) > 1e-3 .or. v(3) /= 0) then
    error stop 1
end if
if (abs(q(1) + 36) > 1e-3 .or. abs(q(2) - 94) > 1e-3 &
        .or. abs(q(3) - 134) > 1e-3) error stop 2
if (any(n /= [4, 2, 1])) error stop 3
do r = 1, 3
    do c = 1, 3
        expect = 0
        if (c <= r) expect = sum(a(r,1:5:2)) * c
        if (abs(s4(r,c) - expect) > 1e-3) error stop 4
        expect = 0
        if (c /= 2) expect = sum(a(r,:)) * c
        if (abs(s1(r,c) - expect) > 1e-3) error stop 5
        if (abs(t1(r,c) - (sum(a(r,1:5:2)) + c)) > 1e-3) error stop 6
        expect = -7
        if (r <= 2 .and. c >= r .and. c <= 2) expect = sum(a(r,1:2*r))
        if (abs(z1(r,c) - expect) > 1e-3) error stop 7
        expect = -7
        if (r <= 2 .and. c >= r .and. c <= 2) expect = sum(a(r,1:2*r))
        if (abs(z2(r,c) - expect) > 1e-3) error stop 8
        if (z3(r,c) /= -7) error stop 9
        expect = -1
        if (c + r <= 3) expect = sum(a(r,1:2*r))
        if (abs(y(r,c) - expect) > 1e-3) error stop 10
        do d = 1, 3
            expect = 0
            if (d >= c) expect = sum(a(r,:)) * c
            if (abs(s3(r,c,d) - expect) > 1e-3) error stop 11
            expect = -1
            if (d >= c + r) expect = sum(a(r,1:2*r)) * d
            if (abs(u(r,c,d) - expect) > 1e-3) error stop 12
            expect = 0
            if (d <= c .and. mod(d, 2) == 1) expect = sum(a(r,2:5)) + c
            if (abs(w(r,c,d) - expect) > 1e-3) error stop 13
        end do
    end do
end do
end program
