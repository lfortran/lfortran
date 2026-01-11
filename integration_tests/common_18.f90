! Test: COMMON block with reordered members
! This tests storage association with different member orderings
! Main: real(8), int, int vs Subroutine: int, int, real(8)
program common_18
    implicit none
    real(8) :: r8
    integer :: i1, i2
    common /data/ r8, i1, i2

    r8 = 3.14159d0
    i1 = 42
    i2 = 99

    call sub_reordered()
    print *, "PASS: common_18"
end program

subroutine sub_reordered()
    implicit none
    integer :: j1, j2
    real(8) :: s8
    common /data/ j1, j2, s8

    ! j1 overlaps first 4 bytes of r8, j2 overlaps second 4 bytes of r8
    ! s8 overlaps i1 and i2 (reinterpreted as real(8))
    ! Just verify we can access without crashing - values will be reinterpreted
    j1 = 1
    j2 = 2
    s8 = 1.0d0
end subroutine
