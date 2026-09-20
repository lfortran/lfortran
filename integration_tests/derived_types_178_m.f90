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

    pure function h(c) result(r)
        character(len=*), intent(in) :: c
        type(ts) :: r
        r%s = c
        r%n = len(c)
    end function

    function g(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        ncalls = ncalls + 1
        r%v = 2*a
    end function

end module
