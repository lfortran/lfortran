module class_84_mod
    implicit none

    type :: string_t
        character(len=:), allocatable :: s
    end type

    type :: temp
        type(string_t), allocatable :: strs(:)
    end type

    type :: dependency_config_t
        type(temp), allocatable :: arr(:)
    end type dependency_config_t

    type, extends(dependency_config_t) :: dependency_node_t
    end type dependency_node_t

end module class_84_mod

program class_84
    use class_84_mod
    implicit none

    type(dependency_node_t), allocatable :: a, b

    !-------------------------
    ! Allocate scalars
    !-------------------------
    allocate(a, b)

    !-------------------------
    ! Initialize 'a'
    !-------------------------
    allocate(a%arr(2))

    allocate(a%arr(1)%strs(2))
    allocate(character(len=5) :: a%arr(1)%strs(1)%s)
    allocate(character(len=3) :: a%arr(1)%strs(2)%s)

    a%arr(1)%strs(1)%s = "hello"
    a%arr(1)%strs(2)%s = "abc"

    allocate(a%arr(2)%strs(1))
    allocate(character(len=4) :: a%arr(2)%strs(1)%s)
    a%arr(2)%strs(1)%s = "test"

    !-------------------------
    ! Assignment under test
    !-------------------------
    b = a

    !-------------------------
    ! Validation: allocation
    !-------------------------
    if (.not. allocated(b%arr)) error stop "b%arr not allocated"
    if (size(b%arr) /= 2) error stop "b%arr wrong size"

    if (.not. allocated(b%arr(1)%strs)) error stop "b%arr(1)%strs not allocated"
    if (size(b%arr(1)%strs) /= 2) error stop "b%arr(1)%strs wrong size"

    if (.not. allocated(b%arr(1)%strs(1)%s)) error stop "string not allocated"
    if (len(b%arr(1)%strs(1)%s) /= 5) error stop "string length mismatch"

    !-------------------------
    ! Validation: values
    !-------------------------
    if (b%arr(1)%strs(1)%s /= "hello") error stop "value copy failed"
    if (b%arr(1)%strs(2)%s /= "abc")   error stop "value copy failed"
    if (b%arr(2)%strs(1)%s /= "test")  error stop "value copy failed"

    !-------------------------
    ! Validation: deep copy
    !-------------------------
    a%arr(1)%strs(1)%s = "xxxxx"
    if (b%arr(1)%strs(1)%s == "xxxxx") error stop "shallow copy detected"

    print *, "OK: extended-type allocatable assignment is correct"

end program class_84
