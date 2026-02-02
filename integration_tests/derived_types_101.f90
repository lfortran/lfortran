program derived_types_101
    implicit none

    type :: base
        integer, allocatable :: s(:)
    end type base

    type, extends(base) :: string_t
        integer, allocatable :: keys(:)
    end type string_t

    type(string_t) :: arg

    print *, "First call"
    call new(arg)

    print *, "Second call"
    call new(arg)

contains

    subroutine new(args)
        type(string_t), intent(out) :: args

        if (allocated(args%s)) error stop "ERROR: args%s is allocated"
        if (allocated(args%keys)) error stop "ERROR: args%keys is allocated"

        allocate(args%s(10))
        allocate(args%keys(10))

    end subroutine new

end program derived_types_101
