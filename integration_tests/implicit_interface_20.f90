! Regression test: allocatable array passed to a procedure with an implicit
! interface must not cause an LLVM type mismatch.
! Reduced from LAPACK TESTING/LIN/schkaa.F (issue #7304 pattern).
program implicit_interface_20
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp), allocatable :: work(:)
    external :: fill_array

    allocate(work(3))
    work = 0.0_dp

    ! Passing allocatable to an implicit interface used to trigger an LLVM
    ! verify error (callee expects pointer-to-pointer but caller passes pointer).
    call fill_array(work, 3)

    if (abs(work(1) - 1.0_dp) < 1.0e-12_dp) then
        if (abs(work(3) - 3.0_dp) < 1.0e-12_dp) then
            print *, "PASS"
        else
            print *, "FAIL"
        end if
    else
        print *, "FAIL"
    end if
end program implicit_interface_20

subroutine fill_array(arr, n)
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, intent(in) :: n
    real(dp), intent(out) :: arr(*)
    integer :: i

    do i = 1, n
        arr(i) = real(i, dp)
    end do
end subroutine fill_array
