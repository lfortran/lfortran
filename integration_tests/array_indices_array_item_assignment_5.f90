module array_indices_array_item_assignment_5_mod
    implicit none
    type :: item_t
        integer, allocatable :: deps(:)
        integer, allocatable :: mat(:,:)
    end type
    type :: holder_t
        type(item_t) :: inner
    end type
contains
    subroutine fill_out(res, src)
        type(item_t), intent(out) :: res
        integer, intent(in) :: src(:)
        res%deps = src([4, 2])
    end subroutine
end module

program array_indices_array_item_assignment_5
    use array_indices_array_item_assignment_5_mod
    implicit none

    integer, allocatable :: rev(:)
    integer :: fixed_arr(4) = [10, 20, 30, 40]
    integer :: mat_src(3, 3)
    integer :: idx(3)
    type(item_t) :: s
    type(item_t) :: r(2)
    type(holder_t) :: h
    integer :: i, j

    allocate(rev(4))
    rev = [4, 3, 2, 1]

    ! 1. Scalar derived type: unallocated component
    s%deps = fixed_arr([2, 4])
    if (size(s%deps) /= 2) error stop
    if (any(s%deps /= [20, 40])) error stop

    ! 2. Scalar derived type: preallocated component (larger -> smaller)
    deallocate(s%deps)
    allocate(s%deps(5))
    s%deps = 0
    s%deps = fixed_arr([3, 1])
    if (size(s%deps) /= 2) error stop
    if (any(s%deps /= [30, 10])) error stop

    ! 3. Derived-type array element: unallocated component
    r(1)%deps = rev([2, 4])
    if (size(r(1)%deps) /= 2) error stop
    if (any(r(1)%deps /= [3, 1])) error stop

    ! 4. Derived-type array element: preallocated component (larger -> smaller)
    allocate(r(2)%deps(5))
    r(2)%deps = 0
    r(2)%deps = rev([2, 4])
    if (size(r(2)%deps) /= 2) error stop
    if (any(r(2)%deps /= [3, 1])) error stop

    ! 5. Derived-type array element: preallocated component (smaller -> larger) with variable index vector
    idx = [4, 2, 1]
    r(1)%deps = rev(idx)
    if (size(r(1)%deps) /= 3) error stop
    if (any(r(1)%deps /= [1, 3, 4])) error stop

    ! 6. 2D allocatable component with vector subscripts
    do j = 1, 3
        do i = 1, 3
            mat_src(i, j) = i * 10 + j
        end do
    end do
    allocate(r(1)%mat(4, 4))
    r(1)%mat = 0
    r(1)%mat = mat_src([3, 1], [2, 3])
    if (size(r(1)%mat, 1) /= 2 .or. size(r(1)%mat, 2) /= 2) error stop
    if (r(1)%mat(1, 1) /= 32 .or. r(1)%mat(2, 1) /= 12) error stop
    if (r(1)%mat(1, 2) /= 33 .or. r(1)%mat(2, 2) /= 13) error stop

    ! 7. Nested derived-type component
    allocate(h%inner%deps(4))
    h%inner%deps = rev([2, 4])
    if (size(h%inner%deps) /= 2) error stop
    if (any(h%inner%deps /= [3, 1])) error stop

    ! 8. intent(out) derived-type component
    call fill_out(s, rev)
    if (size(s%deps) /= 2) error stop
    if (any(s%deps /= [1, 3])) error stop
end program array_indices_array_item_assignment_5
