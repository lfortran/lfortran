module unit_ring_m
    use semiring_m, only: semiring

    implicit none
    private
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
        require :: unit_ring_only_negate(T, plus, zero, mult, one, negate)
    end requirement

    template derive_unit_ring_from_minus(T, plus, zero, mult, one, minus)
        require :: unit_ring_only_minus(T, plus, zero, mult, one, minus)

        private
        public :: negate

    contains
        elemental function negate(x) result(negated)
            type(T), intent(in) :: x
            type(T) :: negated

            negated = minus(zero(), x)
        end function
    end template

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

program template_unitring
implicit none
    
end program