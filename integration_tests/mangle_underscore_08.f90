! Bindings of an extension type in another module override the bindings of
! its parent type with --all-mangling --mangle-underscore (#17305)
module mangle_underscore_08_base
    implicit none
    type, abstract :: a_t
    contains
        procedure(f_i), deferred :: f
    end type
    type :: b_t
    contains
        procedure :: g => b_g
        procedure :: add => b_add
        generic :: operator(+) => add
    end type
    abstract interface
        integer function f_i(s)
            import :: a_t
            class(a_t), intent(in) :: s
        end function
    end interface
contains
    integer function b_g(s)
        class(b_t), intent(in) :: s
        b_g = 1
    end function
    integer function b_add(s, i)
        class(b_t), intent(in) :: s
        integer, intent(in) :: i
        b_add = 100 + i
    end function
end module

module mangle_underscore_08_ext
    use mangle_underscore_08_base, only: a_t, b_t
    implicit none
    type, extends(a_t) :: c_t
    contains
        procedure :: f => c_f
    end type
    type, extends(b_t) :: d_t
    contains
        procedure :: g => d_g
        procedure :: add => d_add
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
    integer function d_add(s, i)
        class(d_t), intent(in) :: s
        integer, intent(in) :: i
        d_add = 200 + i
    end function
end module

program mangle_underscore_08
    use mangle_underscore_08_base, only: a_t, b_t
    use mangle_underscore_08_ext, only: c_t, d_t
    implicit none
    class(a_t), allocatable :: x
    class(b_t), allocatable :: y
    allocate(c_t :: x)
    allocate(d_t :: y)
    print *, x%f(), y%g(), y + 4
    if (x%f() /= 2) error stop 1
    if (y%g() /= 3) error stop 2
    if (y + 4 /= 204) error stop 3
end program
