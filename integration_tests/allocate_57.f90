! Test: allocate with source= for a derived type containing
! a polymorphic allocatable array component (class(T), allocatable :: a(:)).
! Previously caused a segfault because the destination class array elements
! had uninitialized vtable and data pointers during deep copy.
program allocate_57
    implicit none

    type :: base_t
        integer :: val = 0
    end type base_t

    type :: container_t
        class(base_t), allocatable :: items(:)
    end type container_t

    type(container_t), allocatable :: x
    type(container_t) :: y

    ! Test 1: allocate with source= from a function result
    allocate(x, source = make_container(3))
    if (.not. allocated(x)) error stop
    if (.not. allocated(x%items)) error stop
    if (size(x%items) /= 3) error stop
    if (x%items(1)%val /= 10) error stop
    if (x%items(2)%val /= 20) error stop
    if (x%items(3)%val /= 30) error stop
    print *, "Test 1 passed: allocate with source= from function"

    ! Test 2: allocate with source= from a variable
    y = make_container(2)
    deallocate(x)
    allocate(x, source = y)
    if (size(x%items) /= 2) error stop
    if (x%items(1)%val /= 10) error stop
    if (x%items(2)%val /= 20) error stop
    print *, "Test 2 passed: allocate with source= from variable"

    ! Test 3: verify deep copy (modifying source doesn't affect copy)
    y%items(1)%val = 999
    if (x%items(1)%val /= 10) error stop
    print *, "Test 3 passed: deep copy independence"

    print *, "All tests passed!"

contains

    function make_container(n) result(r)
        integer, intent(in) :: n
        type(container_t) :: r
        integer :: i
        allocate(r%items(n))
        do i = 1, n
            r%items(i)%val = i * 10
        end do
    end function make_container

end program allocate_57
