! Test: reshape() with pad argument where target > source at module level
! Verifies that module-level reshape with padding correctly computes the
! compile-time value without corrupting preceding parameter arrays.
module arrays_reshape_35_mod
    implicit none
    real, parameter :: d1(1) = [1]
    real :: d2(2) = reshape(d1, [2], [real:: 0])
end module

program arrays_reshape_35
    use arrays_reshape_35_mod
    implicit none

    if (abs(d2(1) - 1.0) > 1e-6) error stop
    if (abs(d2(2) - 0.0) > 1e-6) error stop

    print *, "ok"
end program
