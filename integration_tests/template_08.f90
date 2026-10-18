module template_08_m
    implicit none

    template vector_template {n}
        deferred integer, parameter :: n
        private
        public :: vector, copy_vector, wrap_vector

        type :: vector
            integer :: elements(n)
        end type
    contains
        pure function copy_vector(x) result(y)
            type(vector), intent(in) :: x
            type(vector) :: y
            y = x
        end function

        pure function wrap_vector(x) result(y)
            type(vector), intent(in) :: x
            type(vector) :: y
            y = copy_vector(copy_vector(x))
        end function
    end template
end module

program template_08
    use template_08_m
    implicit none
    instantiate vector_template {2}, only: vector2 => vector, wrap2 => wrap_vector
    instantiate vector_template {3}, only: vector3 => vector, wrap3 => wrap_vector
    type(vector2) :: a, b
    type(vector3) :: c, d
    a%elements = [11, 22]
    c%elements = [33, 44, 55]
    b = wrap2(a)
    d = wrap3(c)
    if (any(b%elements /= [11, 22])) error stop 1
    if (any(d%elements /= [33, 44, 55])) error stop 2
    if (size(b%elements) /= 2) error stop 3
    if (size(d%elements) /= 3) error stop 4
end program
