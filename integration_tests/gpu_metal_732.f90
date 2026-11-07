module gpu_metal_732_mod
! An array whose bounds are set on entry (an automatic array) and that each
! iteration of a do concurrent loop defines whole by a full-extent section
! is private to the iteration, as when it is defined by `t = v`
! (lfortran/lfortran#17013). `t(:)` has the bounds `lbound(t, d)` and
! `ubound(t, d)`, which have no compile-time value here. The same holds for
! an element loop `do j = lbound(t, 1), ubound(t, 1)` that defines every
! element.
implicit none
contains

subroutine run(m, n, out)
    integer, intent(in) :: m, n
    integer, intent(out) :: out(:)
    integer :: a(n), b(m:n), c(n, 2), d(n), e(m:n), i, j

    do concurrent (i = 1:size(out))
        a(:) = i
        if (mod(i, 7) == 0) a(2) = 0
        out(i) = a(mod(i, n) + 1)
    end do
    do i = 1, size(out)
        if (mod(i, 7) == 0 .and. mod(i, n) == 1) then
            if (out(i) /= 0) error stop 1
        else
            if (out(i) /= i) error stop 2
        end if
    end do

    do concurrent (i = 1:size(out))
        b(:) = 2 * i
        out(i) = b(mod(i, n - m + 1) + m)
    end do
    do i = 1, size(out)
        if (out(i) /= 2 * i) error stop 3
    end do

    do concurrent (i = 1:size(out))
        c(:, :) = 3 * i
        c(mod(i, n) + 1, 2) = -i
        out(i) = c(mod(i, n) + 1, 1) + c(mod(i + 1, n) + 1, 2)
    end do
    do i = 1, size(out)
        if (out(i) /= 6 * i) error stop 4
    end do

    do concurrent (i = 1:size(out))
        d(lbound(d, 1):ubound(d, 1)) = 4 * i
        out(i) = d(mod(i, n) + 1)
    end do
    do i = 1, size(out)
        if (out(i) /= 4 * i) error stop 5
    end do

    do concurrent (i = 1:size(out))
        do j = lbound(e, 1), ubound(e, 1)
            e(j) = i + j
        end do
        out(i) = e(mod(i, n - m + 1) + m) - (mod(i, n - m + 1) + m)
    end do
    do i = 1, size(out)
        if (out(i) /= i) error stop 6
    end do
end subroutine

end module

program gpu_metal_732
use gpu_metal_732_mod, only: run
implicit none
integer :: out(10000)
call run(2, 5, out)
print *, "ok"
end program
