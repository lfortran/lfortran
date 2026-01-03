program class_83
    implicit none

    type :: string_t
        integer :: dummy = -1
    end type

    type :: preprocess_t
        type(string_t), allocatable :: strs(:)
    end type

    type :: temp
        type(preprocess_t), allocatable :: arr(:)
    end type

    type(temp), allocatable :: x(:), z(:)

    !-------------------------
    ! Allocate top-level arrays
    !-------------------------
    allocate(x(4))
    allocate(z(4))

    !-------------------------
    ! Initialize x(1)
    !-------------------------
    allocate(x(1)%arr(2))
    allocate(x(1)%arr(1)%strs(3))
    allocate(x(1)%arr(2)%strs(1))

    x(1)%arr(1)%strs(1)%dummy = 10
    x(1)%arr(1)%strs(2)%dummy = 10
    x(1)%arr(1)%strs(3)%dummy = 10
    x(1)%arr(2)%strs(1)%dummy = 20

    z(1) = x(1)

    if (.not. allocated(z(1)%arr)) error stop "z(1)%arr not allocated"
    if (size(z(1)%arr) /= 2) error stop "z(1)%arr wrong size"

    if (.not. allocated(z(1)%arr(1)%strs)) error stop "z(1)%arr(1)%strs not allocated"
    if (size(z(1)%arr(1)%strs) /= 3) error stop "z(1)%arr(1)%strs wrong size"

    if (z(1)%arr(1)%strs(1)%dummy /= 10) error stop "z copy failed"

    x(1) = z(1)

    if (.not. allocated(x(1)%arr)) error stop "x(1)%arr not allocated after copy back"
    if (size(x(1)%arr) /= 2) error stop "x(1)%arr wrong size after copy back"

    if (x(1)%arr(2)%strs(1)%dummy /= 20) error stop "x copy-back failed"

    print *, "OK: deep allocatable assignment works correctly"

end program class_83
