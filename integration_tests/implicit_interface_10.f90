! Test for LLVM type mismatch when passing allocatable array to
! subroutine with implicit interface
! Related to issue #7304
! Reduced from LAPACK TESTING/LIN/schkaa.F
program implicit_interface_10
    implicit none
    real, allocatable :: work(:)
    external fill_array

    allocate(work(3))
    work = 0.0

    ! Passing allocatable to implicit interface triggers LLVM verify error:
    ! Declaration uses float** but call passes float*
    call fill_array(work, 3)

    if (abs(work(1) - 1.0) < 1e-6 .and. abs(work(3) - 3.0) < 1e-6) then
        print *, "PASS"
    else
        print *, "FAIL"
    end if
end program

subroutine fill_array(arr, n)
    implicit none
    integer, intent(in) :: n
    real, intent(out) :: arr(*)
    integer :: i

    do i = 1, n
        arr(i) = real(i)
    end do
end subroutine
