! An INSTANTIATE without an only-list makes the derived types of the template
! available under their own names (#13728).
module template_instantiate_no_only_01_tmpl
    implicit none
    private
    public :: matrix_tmpl
    template matrix_tmpl {T, zero_t, n}
        deferred type :: T
        deferred interface
            pure function zero_t() result(z)
                import :: T
                type(T) :: z
            end function
        end interface
        deferred integer, parameter :: n
        type :: matrix
            type(T) :: elements(n, n)
        end type
        type :: z_cell
            type(T) :: x
        end type
        type :: a_row
            type(z_cell) :: cell
        end type
        type :: wrapper
            type(T) :: v
        contains
            procedure :: get
        end type
    contains
        pure function zero() result(r)
            type(matrix) :: r
            r%elements = zero_t()
        end function
        function get(self) result(r)
            class(wrapper), intent(in) :: self
            type(T) :: r
            r = self%v
        end function
    end template
end module

module template_instantiate_no_only_01_real
    use template_instantiate_no_only_01_tmpl, only: matrix_tmpl
    implicit none
    integer, parameter :: n = 2
    instantiate matrix_tmpl {real, real_zero, n}
contains
    pure function real_zero()
        real :: real_zero
        real_zero = 0.
    end function
end module

program template_instantiate_no_only_01
    use template_instantiate_no_only_01_real
    implicit none
    type(matrix) :: m
    type(a_row) :: r
    type(z_cell) :: c
    type(wrapper) :: w
    m%elements = 1.
    m = zero()
    if (any(m%elements /= 0.)) error stop
    if (size(m%elements) /= 4) error stop
    c%x = 3.
    r%cell = c
    if (r%cell%x /= 3.) error stop
    w%v = 7.
    if (w%get() /= 7.) error stop
    if (get(w) /= 7.) error stop
    print *, m%elements, r%cell%x, w%get()
end program
