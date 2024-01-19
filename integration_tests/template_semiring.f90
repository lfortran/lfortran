module semiring_m
    use monoid_m, only: monoid

    implicit none
    private
    public :: semiring

    requirement semiring(T, plus, zero, mult, one)
        require :: monoid(T, plus, zero)
        require :: monoid(T, mult, one)
    end requirement
end module