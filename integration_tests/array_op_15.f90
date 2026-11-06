! A section assignment whose value reads the same elements of the target,
! through an intrinsic elemental function or a pure elemental call, is
! carried out element by element. A value that reads other elements of the
! target, as a shifted section does, is left to the temporary path.
module array_op_15_mod
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
contains

    pure elemental function halve(x) result(y)
        real(real32), intent(in) :: x
        real(real32) :: y

        y = 0.5_real32*x
    end function halve

    pure subroutine apply(scores, visible, total)
        real(real32), contiguous, intent(inout) :: scores(:)
        real(real32), intent(in) :: total
        integer, intent(in) :: visible
        real(real32) :: m

        m = maxval(scores(:visible))
        scores(:visible) = exp(scores(:visible)-m)
        scores(:visible) = halve(scores(:visible)/total)
        scores(2:visible) = exp(scores(1:visible-1))
    end subroutine apply

end module array_op_15_mod

program array_op_15
    use, intrinsic :: iso_fortran_env, only: real32
    use array_op_15_mod, only: apply
    implicit none
    integer, parameter :: n = 9
    real(real32) :: a(n), expected(n), total
    real(real32) :: shifted(n), m
    integer :: i, visible

    visible = 7
    total = 1.25_real32
    do i = 1, n
        a(i) = real(i, kind=real32)/4.0_real32
    end do

    expected = a
    m = maxval(expected(:visible))
    do i = 1, visible
        expected(i) = exp(expected(i)-m)
    end do
    do i = 1, visible
        expected(i) = 0.5_real32*(expected(i)/total)
    end do
    shifted = expected
    do i = 2, visible
        expected(i) = exp(shifted(i-1))
    end do

    call apply(a, visible, total)

    do i = 1, n
        if (a(i) /= expected(i)) error stop "same-element section mismatch"
    end do
end program array_op_15
