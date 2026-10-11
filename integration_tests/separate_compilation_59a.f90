! The bound of a dummy and of a function result is a module variable of
! another module, read through a generated getter of this module that other
! translation units call.
module separate_compilation_59_sizes
implicit none
integer :: n = 2
end module

module separate_compilation_59a
implicit none
contains
    function make() result(r)
        use separate_compilation_59_sizes, only: n
        real :: r(n)
        r = 3.0
    end function

    subroutine fill(a)
        use separate_compilation_59_sizes, only: n
        real :: a(n)
        a = 42.0
    end subroutine
end module
