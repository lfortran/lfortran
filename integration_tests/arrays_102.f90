program arrays_102
    implicit none

    type :: string_t
        character(len=:), allocatable :: s
    end type string_t

    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp

    temp%s = "build"
    allocate(build_dirs(0))
    associate(xx => temp)
        build_dirs = [build_dirs, temp]
    end associate

    if (size(build_dirs) /= 1) error stop "Wrong size"
    if (build_dirs(1)%s /= "build") error stop "Wrong value"

    print *, "OK:", build_dirs(1)%s
end program arrays_102
