module mangle_underscore_09_m
    implicit none
    type, abstract :: a_t
    contains
        procedure(f_i), deferred :: f
    end type
    type :: b_t
    contains
        procedure :: g => b_g
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
end module
