! Test for double-free bug when passing allocatable array element to
! assumed-size array in a loop. This pattern is common in LAPACK where
! routines like SGEBRD call SGEBD2 with A(I,I) as the start of a submatrix.
!
! The bug manifests as "double free detected" at program exit when
! --legacy-array-sections flag is used.
!
! Related to LAPACK XERBLA errors where wrong routines appear to call
! XERBLA with incorrect parameters.
program legacy_array_sections_12
    implicit none
    real, allocatable :: a(:,:)
    integer :: i, info

    allocate(a(10, 10))
    a = 1.0

    ! This loop triggers the double-free bug
    do i = 1, 3
        call check_subarray(10-i+1, a(i, i), 10, info)
        if (info /= 0) error stop
    end do

    deallocate(a)
    print *, "PASSED: No double-free"

end program

subroutine check_subarray(m, a, lda, info)
    ! Subroutine with assumed-size array, like LAPACK SGEBD2
    implicit none
    integer, intent(in) :: m, lda
    real, intent(inout) :: a(lda, *)
    integer, intent(out) :: info

    ! Check LDA like LAPACK does
    info = 0
    if (m < 0) then
        info = -1
    else if (lda < max(1, m)) then
        info = -3
    end if

    if (info /= 0) return

    ! Access array to ensure it is valid
    a(1, 1) = a(1, 1) + 0.1

end subroutine
