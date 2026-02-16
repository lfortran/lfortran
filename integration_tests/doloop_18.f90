! Test that do loop upper bounds are evaluated only once (Fortran standard).
! The upper bound contains a merge() call depending on a variable modified
! by a recursive call inside the loop body.
program doloop_18
    implicit none
    integer :: k, total

    k = 2
    total = 0
    call test(4, 4, k, total)
    if (total /= 8) error stop

contains

    recursive subroutine test(ji, jf, k, total)
        integer, intent(in)    :: ji
        integer, intent(in)    :: jf
        integer, intent(inout) :: k
        integer, intent(inout) :: total

        integer :: n, jt

        if (ji == 1) then
            k = 1
        else
            jt = ji - 1
            do n = 1, merge(k, 1, jt /= 1)
                total = total + 1
                call test(jt, jf, k, total)
            end do
            if (ji == jf) k = 2
        end if
    end subroutine test

end program doloop_18
