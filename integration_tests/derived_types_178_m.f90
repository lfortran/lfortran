module derived_types_178_m
implicit none

type :: tt
    real, allocatable :: v(:)
end type

integer :: ncalls = 0

contains

    pure function f(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        r%v = a
    end function

    function g(a) result(r)
        real, intent(in) :: a(:)
        type(tt) :: r
        ncalls = ncalls + 1
        r%v = 2*a
    end function

end module
