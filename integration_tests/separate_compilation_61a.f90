! The interface of a procedure-pointer component and of a procedure pointer
! bounds an array by a module variable of another module, read through a
! generated getter that other translation units reach via the modfile (#14326).
module separate_compilation_61_sizes
implicit none
integer :: n = 2
end module

module separate_compilation_61a
implicit none
type t
    procedure(callback), nopass, pointer :: f => null()
end type

abstract interface
    subroutine callback(x, a)
        use separate_compilation_61_sizes, only: n
        import t
        class(t) :: x
        real :: a(n)
    end subroutine

    function fcb(k) result(r)
        use separate_compilation_61_sizes, only: n
        integer, intent(in) :: k
        real :: r(n)
    end function
end interface
contains
    subroutine impl(x, a)
        use separate_compilation_61_sizes, only: n
        class(t) :: x
        real :: a(n)
        a = 5.0
    end subroutine

    function fimpl(k) result(r)
        use separate_compilation_61_sizes, only: n
        integer, intent(in) :: k
        real :: r(n)
        r = real(k)
    end function
end module
