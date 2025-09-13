!This file contains tests for implied do loops in array initializers.
program implied_do_loop12
    implicit none
    integer :: k
    integer, parameter :: MAX_ITEMS = 3
    ! Derived type
    type :: derived_t
        real :: a
    end type derived_t
    ! Array of integers initialized via implied-DO
    integer :: arr1(MAX_ITEMS) = (/ (k, k = 0, MAX_ITEMS-1) /)
    type(derived_t) :: arr2(MAX_ITEMS) = (/ (derived_t(k), k = 0, MAX_ITEMS-1) /)
    print *,arr1
    if(arr1(1) /= 0) error stop
    if(arr1(2) /= 1) error stop
    if(arr1(3) /= 2) error stop
    ! Array of derived_t objects initialized via implied-DO
    print *, arr2
    if (abs(arr2(1)%a - 0.0) > 1e-12) error stop
    if (abs(arr2(2)%a - 1.0) > 1e-12) error stop
    if (abs(arr2(3)%a - 2.0) > 1e-12) error stop
end program implied_do_loop12