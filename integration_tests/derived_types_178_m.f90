module derived_types_178_m
implicit none

type :: tt
    real, allocatable :: v(:)
end type

type :: ts
    character(len=8) :: s
    integer :: n
end type

integer :: ncalls = 0

contains

    pure function f(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        r%v = a
    end function

    ! The result is scribbled over before the argument is read, so if the
    ! target of the assignment is passed in as the result there is nothing
    ! left to read.
    pure function h(c) result(r)
        character(len=*), intent(in) :: c
        type(ts) :: r
        r%s = "ZZZZZZZZ"
        r%n = len(c)
        r%s = c
    end function

    function g(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        ncalls = ncalls + 1
        r%v = 2*a
    end function

end module
