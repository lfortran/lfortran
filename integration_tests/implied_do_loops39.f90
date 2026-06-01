! Test: implied do loop with struct-typed array elements in read/write.
! Regression test for ICE: visit_ImpliedDoLoop() not implemented
! when struct members were incorrectly wrapped around ImpliedDoLoop
! instead of being expanded inside the loop values.
program implied_do_loops39
    implicit none

    type :: point_t
        real(8) :: x, y, z
    end type

    type(point_t) :: pts(3)
    integer :: i, n

    ! --- Test 1: constant-bound implied do loop write/read with struct ---
    pts(1) = point_t(1.0d0, 2.0d0, 3.0d0)
    pts(2) = point_t(4.0d0, 5.0d0, 6.0d0)
    pts(3) = point_t(7.0d0, 8.0d0, 9.0d0)

    open(10, status="scratch", form="formatted")
    write(10, *) (pts(i), i = 1, 3)
    rewind(10)

    pts = point_t(0.0d0, 0.0d0, 0.0d0)
    read(10, *) (pts(i), i = 1, 3)
    close(10)

    if (abs(pts(1)%x - 1.0d0) > 1.0d-10) error stop "Test 1 failed: pts(1)%x"
    if (abs(pts(1)%y - 2.0d0) > 1.0d-10) error stop "Test 1 failed: pts(1)%y"
    if (abs(pts(1)%z - 3.0d0) > 1.0d-10) error stop "Test 1 failed: pts(1)%z"
    if (abs(pts(2)%x - 4.0d0) > 1.0d-10) error stop "Test 1 failed: pts(2)%x"
    if (abs(pts(2)%y - 5.0d0) > 1.0d-10) error stop "Test 1 failed: pts(2)%y"
    if (abs(pts(2)%z - 6.0d0) > 1.0d-10) error stop "Test 1 failed: pts(2)%z"
    if (abs(pts(3)%x - 7.0d0) > 1.0d-10) error stop "Test 1 failed: pts(3)%x"
    if (abs(pts(3)%y - 8.0d0) > 1.0d-10) error stop "Test 1 failed: pts(3)%y"
    if (abs(pts(3)%z - 9.0d0) > 1.0d-10) error stop "Test 1 failed: pts(3)%z"

    ! --- Test 2: variable-bound implied do loop write/read with struct ---
    ! This is the case that triggered the original ICE.
    pts(1) = point_t(10.0d0, 20.0d0, 30.0d0)
    pts(2) = point_t(40.0d0, 50.0d0, 60.0d0)

    n = 2
    open(10, status="scratch", form="formatted")
    write(10, *) (pts(i), i = 1, n)
    rewind(10)

    pts(1) = point_t(0.0d0, 0.0d0, 0.0d0)
    pts(2) = point_t(0.0d0, 0.0d0, 0.0d0)
    read(10, *) (pts(i), i = 1, n)
    close(10)

    if (abs(pts(1)%x - 10.0d0) > 1.0d-10) error stop "Test 2 failed: pts(1)%x"
    if (abs(pts(1)%y - 20.0d0) > 1.0d-10) error stop "Test 2 failed: pts(1)%y"
    if (abs(pts(1)%z - 30.0d0) > 1.0d-10) error stop "Test 2 failed: pts(1)%z"
    if (abs(pts(2)%x - 40.0d0) > 1.0d-10) error stop "Test 2 failed: pts(2)%x"
    if (abs(pts(2)%y - 50.0d0) > 1.0d-10) error stop "Test 2 failed: pts(2)%y"
    if (abs(pts(2)%z - 60.0d0) > 1.0d-10) error stop "Test 2 failed: pts(2)%z"

    print *, "All tests passed."
end program implied_do_loops39
