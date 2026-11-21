! Bindings of an extension type override the bindings of its parent type
! from a module compiled separately with --all-mangling --mangle-underscore
! (#17305)
module mangle_underscore_09_ext
    use mangle_underscore_09_m, only: a_t, b_t
    implicit none
    type, extends(a_t) :: c_t
    contains
        procedure :: f => c_f
    end type
    type, extends(b_t) :: d_t
    contains
        procedure :: g => d_g
    end type
contains
    integer function c_f(s)
        class(c_t), intent(in) :: s
        c_f = 2
    end function
    integer function d_g(s)
        class(d_t), intent(in) :: s
        d_g = 3
    end function
end module

program mangle_underscore_09
    use mangle_underscore_09_m, only: a_t, b_t
    use mangle_underscore_09_ext, only: c_t, d_t
    implicit none
    class(a_t), allocatable :: x
    class(b_t), allocatable :: y
    allocate(c_t :: x)
    allocate(d_t :: y)
    print *, x%f(), y%g()
    if (x%f() /= 2) error stop 1
    if (y%g() /= 3) error stop 2
end program
