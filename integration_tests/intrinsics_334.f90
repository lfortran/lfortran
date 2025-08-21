program intrinsics_334
    implicit none

    type, abstract :: base
     integer :: id
    end type
    type, extends(base) :: toml_value
        integer :: x = 0
    end type toml_value

    integer :: i
    integer, allocatable :: from(:), to(:)
    type(toml_value), allocatable :: struct_from, struct_to
    type(toml_value), allocatable :: struct_from2(:), struct_to2(:)
    class(toml_value), allocatable :: struct_from3
    class(base), allocatable :: struct_to3

    allocate(from(5))
    from = [1, 2, 3, 4, 5]

    allocate(to(5))
    call move_alloc(from, to)
    print *, to
    if(any(to /= [1,2,3,4,5])) error stop

    print *, allocated(from)
    if(allocated(from) .neqv. .false.) error stop 

    allocate(struct_from)
    struct_from%x = 42
    call move_alloc(struct_from, struct_to)
    if (allocated(struct_to)) then
        if (struct_to%x /= 42) error stop
    else
        error stop
    end if
    if (allocated(struct_from)) error stop

    allocate(struct_from2(3))
    do i = 1, 3
        struct_from2(i)%x = i * 10
    end do
    call move_alloc(struct_from2, struct_to2)
    if (allocated(struct_from2)) error stop
    if (allocated(struct_to2)) then
        do i = 1, size(struct_to2)
            if (struct_to2(i)%x /= i * 10) error stop
        end do
    else
        error stop
    end if

    allocate(toml_value :: struct_from3)
    struct_from3%x = 123

    call move_alloc(struct_from3, struct_to3)
    if (allocated(struct_from3)) error stop

    select type(struct_to3)
    type is (toml_value)
        if (struct_to3%x /= 123) error stop
    end select

    allocate(struct_from)
    struct_from%x = 42
    call move_alloc(struct_from, struct_to3)
    if (allocated(struct_from)) error stop

    select type(struct_to3)
    type is (toml_value)
        if (struct_to3%x /= 42) error stop
    class default
        error stop
    end select
end program
