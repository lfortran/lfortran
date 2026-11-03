! An internal procedure calls its recursive host again, then reads the host
! variables of its own instance, directly and through a dummy argument whose
! actual argument is a host variable (#15507). g in t modifies k through j,
! so it refers to k only through j (Fortran 2018 15.5.2.13); t checks k.
module host_assoc_01_m
implicit none
integer :: nlines = 0
integer :: lines(3, 3) = 0
contains

recursive subroutine s(n)
    integer, intent(in) :: n
    integer :: k
    k = 10*n
    call g()
contains
    recursive subroutine g()
        integer :: n0, k0
        n0 = n
        k0 = k
        if (n > 1) call s(n - 1)
        nlines = nlines + 1
        lines(:, nlines) = [n0, n, k]
        if (n /= n0) error stop 1
        if (k /= k0) error stop 2
    end subroutine g
end subroutine s

recursive subroutine t(n, total)
    integer, intent(in) :: n
    integer, intent(inout) :: total
    integer :: k
    k = 10*n
    call g(n, k)
    if (k /= 10*n + 1) error stop 3
contains
    recursive subroutine g(m, j)
        integer, intent(in) :: m
        integer, intent(inout) :: j
        integer :: m0
        m0 = m
        if (n > 1) call t(n - 1, total)
        ! m is n and j is k of this instance of t
        if (m /= m0 .or. m /= n) error stop 4
        if (j /= 10*m0) error stop 5
        j = j + 1
        total = total + m
    end subroutine g
end subroutine t

end module host_assoc_01_m

program host_assoc_01
use host_assoc_01_m
implicit none
integer :: total
call s(3)
if (nlines /= 3) error stop 7
if (any(lines(:, 1) /= [1, 1, 10])) error stop 8
if (any(lines(:, 2) /= [2, 2, 20])) error stop 9
if (any(lines(:, 3) /= [3, 3, 30])) error stop 10
total = 0
call t(3, total)
if (total /= 6) error stop 11
print *, "ok"
end program host_assoc_01
