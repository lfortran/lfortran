! Test: reshape of derived type array with allocatable component
! Ensures reshape does a deep copy (not shallow) of allocatable members.
program arrays_reshape_33
    implicit none
    type :: t
        real, allocatable :: v(:)
    end type
    type(t), allocatable :: a(:), b(:,:)

    allocate(a(2))
    a(1)%v = [1.0, 2.0]
    a(2)%v = [3.0, 4.0]

    b = reshape(a, [1, 2])

    ! Verify reshaped values
    if (size(b, 1) /= 1) error stop
    if (size(b, 2) /= 2) error stop
    if (size(b(1,1)%v) /= 2) error stop
    if (size(b(1,2)%v) /= 2) error stop
    if (abs(b(1,1)%v(1) - 1.0) > 1e-6) error stop
    if (abs(b(1,1)%v(2) - 2.0) > 1e-6) error stop
    if (abs(b(1,2)%v(1) - 3.0) > 1e-6) error stop
    if (abs(b(1,2)%v(2) - 4.0) > 1e-6) error stop

    ! Verify source array is unaffected (deep copy, not shared)
    a(1)%v(1) = 99.0
    if (abs(b(1,1)%v(1) - 1.0) > 1e-6) error stop

    print *, "PASS"
end program
