module template_04_semigroup

    implicit none
    private
    public :: semigroup, extended_semigroup, derive_extended_semigroup

    requirement semigroup(T, combine)
        type, deferred :: T
        elemental function combine(x, y) result(combined)
            type(T), intent(in) :: x, y
            type(T) :: combined
        end function
    end requirement

    requirement extended_semigroup(T, combine, sconcat, stimes)
        require :: semigroup(T, combine)
        pure function sconcat(list) result(combined)
            type(T), intent(in) :: list(:) !! Must contain at least one element
            type(T) :: combined
        end function
        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated
        end function
    end requirement

    template derive_extended_semigroup(T, combine)
        require :: semigroup(T, combine)
        private
        public :: sconcat, stimes
    contains
        pure function sconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
            integer :: i
            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                error stop "Attempted to sconcat empty list"
            end if
        end function

        elemental function stimes(n, a) result(repeated)
            integer, intent(in) :: n
            type(T), intent(in) :: a
            type(T) :: repeated
            integer :: i
            if (n < 1) error stop "n must be > 0"
            repeated = a
            do i = 2, n
                repeated = combine(repeated, a)
            end do
        end function
    end template

end module

module template_04_monoid
    ! TODO : allow importing requirements and templates
    use template_04_semigroup
    implicit none
    private
    public :: monoid, extended_monoid, derive_extended_monoid

    requirement monoid(T, combine, empty)
        require :: semigroup(T, combine)
        pure function empty()
            type(T) :: empty
        end function
    end requirement

    requirement extended_monoid(T, combine, sconcat, stimes, empty, mconcat)
        require :: extended_semigroup(T, combine, sconcat, stimes)
        require :: monoid(T, combine, empty)
        pure function mconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
        end function
    end requirement

    template derive_extended_monoid(T, combine, empty)
        require :: monoid(T, combine, empty)
        private
        public :: stimes, mconcat
        instantiate derive_extended_semigroup(T, combine), only: stimes => stimes
    contains
        pure function mconcat(list) result(combined)
            type(T), intent(in) :: list(:)
            type(T) :: combined
            integer :: i
            if (size(list) > 0) then
                combined = list(1)
                do i = 2, size(list)
                    combined = combine(combined, list(i))
                end do
            else
                combined = empty()
            end if
        end function
    end template

end module

module template_04_semiring
    use template_04_monoid

    implicit none
    private
    public :: semiring

    requirement semiring(T, plus, zero, mult, one)
        require :: monoid(T, plus, zero)
        require :: monoid(T, mult, one)
    end requirement
end module

module template_04_unitring
    use template_04_semiring

    implicit none
    public :: &
        unit_ring_only_minus, &
        unit_ring_only_negate, &
        unit_ring, &
        derive_unit_ring_from_minus, &
        derive_unit_ring_from_negate

    requirement unit_ring_only_minus(T, plus, zero, mult, one, minus)
        require :: semiring(T, plus, zero, mult, one)
        elemental function minus(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference
        end function
    end requirement

    requirement unit_ring_only_negate(T, plus, zero, mult, one, negate)
        require :: semiring(T, plus, zero, mult, one)
        elemental function negate(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated
        end function
    end requirement

    requirement unit_ring(T, plus, zero, mult, one, minus, negate)
        require :: unit_ring_only_minus(T, plus, zero, mult, one, minus)
        require ::  unit_ring_only_negate(T, plus, zero, mult, one, negate)
    end requirement

    template derive_unit_ring_from_negate(T, plus, zero, mult, one, negate)
        require :: unit_ring_only_negate(T, plus, zero, mult, one, negate)
        private
        public :: minus
    contains
        elemental function minus(x, y) result(difference)
            type(T), intent(in) :: x, y
            type(T) :: difference
            difference = plus(x, negate(y))
        end function
    end template

end module

module template_04_field
    !! field is a unit_ring that also has a division or inverse operation
    use template_04_unitring
    implicit none
    private
    public :: &
            field_only_division, &
            field_only_inverse, &
            field, &
            derive_field_from_division, &
            derive_field_from_inverse

    requirement field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: unit_ring(T, plus, zero, mult, one, minus, negate)
        elemental function divide(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient
        end function
    end requirement

    requirement field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
        require :: unit_ring(T, plus, zero, mult, one, minus, negate)
        elemental function invert(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse
        end function
    end requirement

    requirement field(T, plus, zero, mult, one, minus, negate, divide, invert)
        require :: field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
    end requirement

    template derive_field_from_division(T, plus, zero, mult, one, minus, negate, divide)
        require :: field_only_division(T, plus, zero, mult, one, minus, negate, divide)
        private
        public :: invert
    contains
        elemental function invert(x) result(inverse)
            type(T), intent(in) :: x
            type(T) :: inverse
            inverse = divide(one(), x)
        end function
    end template

    template derive_field_from_inverse(T, plus, zero, mult, one, minus, negate, invert)
        require :: field_only_inverse(T, plus, zero, mult, one, minus, negate, invert)
        private
        public :: divide
    contains
        elemental function divide_(x, y) result(quotient)
            type(T), intent(in) :: x, y
            type(T) :: quotient
            quotient = mult(x, invert(y))
        end function
    end template
end module

module template_04_matrix

    use template_04_monoid
    use template_04_semiring
    use template_04_unitring

    implicit none
    private
    public :: matrix_tmpl

    template matrix_tmpl(T, plus_t, zero_t, times_t, one_t, n)
        require :: semiring(T, plus_t, zero_t, times_t, one_t)
        integer :: n

        private

        type :: matrix
            type(T) :: elements(n, n)
        end type
    contains
        elemental function plus_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined
            integer :: i, j
            ! TODO: something wrong with elemental function operations
            ! combined%elements = plus_t(x%elements, y%elements)
            do i = 1, n
                do j = 1, n
                    combined%elements(i,j) = plus_t(x%elements(i,j), y%elements(i,j))
                end do
            end do
        end function

        pure function zero()
            type(matrix) :: zero

            zero%elements = zero_t()
        end function

        elemental function times_matrix(x, y) result(combined)
            type(matrix), intent(in) :: x, y
            type(matrix) :: combined

            !instantiate derive_extended_monoid(T, plus_t, zero_t), only: sum => mconcat
            integer :: i, j, k
            type(T) :: dot
            do i = 1, n
                do j = 1, n
                    ! TODO: something wrong with the assignment
                    !combined%elements(i, j) = sum(times_t(x%elements(i,:), y%elements(:,j)))
                    dot = zero_t()
                    do k = 1, n
                        dot = plus_t(dot, times_t(x%elements(i,k), y%elements(k,j)))
                    end do
                    combined%elements(i, j) = dot
                end do
            end do
        end function

        pure function one()
            type(matrix) :: one

            integer :: i

            one%elements = zero_t()
            do concurrent (i = 1:n)
                one%elements(i, i) = one_t()
            end do
        end function
    end template
end module

module template_04_func
    implicit none
    private
    public :: zero_integer, zero_real, one_integer, one_real

contains

    pure function zero_integer() result(z)
        integer :: z
        z = 0
    end function

    pure function one_integer() result(z)
        integer :: z
        z = 1
    end function

    pure function zero_real() result(z)
        real :: z
        z = 0
    end function

    pure function one_real() result(z)
        real :: z
        z = 1
    end function

end module

program template_04
use template_04_matrix
use template_04_func

integer, parameter :: n = 2
instantiate matrix_tmpl(integer, operator(+), zero_integer, operator(*), one_integer, n), &
    only: integer_matrix => matrix, &
          integer_plus_matrix => plus_matrix, &
          integer_times_matrix => times_matrix
          
type(integer_matrix) :: m1, m2, m3, m4
m1%elements(1,1) = 1
m1%elements(1,2) = 0
m1%elements(2,1) = 0
m1%elements(2,2) = 1

m2%elements(1,1) = 1
m2%elements(1,2) = 2
m2%elements(2,1) = 2
m2%elements(2,2) = 1

m3 = integer_plus_matrix(m1, m2)
print *, m3%elements(1,1), m3%elements(1,2)
print *, m3%elements(2,1), m3%elements(2,2), achar(10)

m4 = integer_times_matrix(m3, m2)
print *, m4%elements(1,1), m4%elements(1,2)
print *, m4%elements(2,1), m4%elements(2,2), achar(10)

instantiate matrix_tmpl(real, operator(+), zero_real, operator(*), one_real, n), &
    only: real_matrix => matrix, &
          real_plus_matrix => plus_matrix, &
          real_times_matrix => times_matrix

type(real_matrix) :: r1, r2, r3, r4
r1%elements(1,1) = 1.2
r1%elements(1,2) = 0
r1%elements(2,1) = 0
r1%elements(2,2) = 1

r2%elements(1,1) = 1
r2%elements(1,2) = 2.5
r2%elements(2,1) = 2
r2%elements(2,2) = 1

r3 = real_plus_matrix(r1, r2)
print *, r3%elements(1,1), r3%elements(1,2)
print *, r3%elements(2,1), r3%elements(2,2), achar(10)

r4 = real_times_matrix(r3, r2)
print *, r4%elements(1,1), r4%elements(1,2)
print *, r4%elements(2,1), r4%elements(2,2), achar(10)
end program