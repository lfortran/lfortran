! Names in the kind, length, bounds and default initialization of a component
! are resolved in the host scope even when a component of the same name
! exists: here the host entities are local named constants of the program
! and of a contained procedure.
program derived_types_217
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: count = 7
    integer, parameter :: len = 5, c = 2

    type :: t
        integer :: dp = 2
        real(dp) :: r = 1.5_dp
        integer :: count = count
    end type

    type(t) :: x

    if (x%dp /= 2) error stop
    if (kind(x%r) /= kind(1.0d0)) error stop
    if (abs(x%r - 1.5_dp) > 1e-12_dp) error stop
    if (x%count /= 7) error stop
    call s()
    print *, x%dp, x%r, x%count

contains

    subroutine s()
        integer, parameter :: w = 4
        type :: u
            character(len=len) :: len = "hello"
            integer :: w = w
            integer :: c(c) = [c, c]
        end type
        type(u) :: y

        if (y%len /= "hello") error stop
        if (y%w /= 4) error stop
        if (size(y%c) /= 2) error stop
        if (any(y%c /= 2)) error stop
        print *, y%len, y%w, y%c
    end subroutine
end program
