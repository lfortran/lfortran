module stdlib_bitsets_operator_overloading_9_m
    implicit none
    public :: bitset_type, bitset_large, bitset_64
    public :: operator(==)

    type, abstract :: bitset_type
        private
        integer(4) :: num_bits
    contains
    end type bitset_type

    type, extends(bitset_type) :: bitset_large
    end type bitset_large

    type, extends(bitset_type) :: bitset_64
    end type bitset_64

    interface operator(==)
        elemental module function eqv_large(set1, set2) result(eqv)
            logical                        :: eqv
            type(bitset_large), intent(in) :: set1, set2
        end function eqv_large

        elemental module function eqv_64(set1, set2) result(eqv)
            logical                     :: eqv
            type(bitset_64), intent(in) :: set1, set2
        end function eqv_64
    end interface operator(==)

    contains

    elemental module function eqv_large(set1, set2) result(eqv)
        logical                        :: eqv
        type(bitset_large), intent(in) :: set1, set2
        eqv = .false. ! dummy implementation
    end function eqv_large

    elemental module function eqv_64(set1, set2) result(eqv)
        logical                     :: eqv
        type(bitset_64), intent(in) :: set1, set2
        eqv = .true. ! dummy implementation
    end function eqv_64
end module stdlib_bitsets_operator_overloading_9_m

program operator_overloading_09
  use stdlib_bitsets_operator_overloading_9_m
  implicit none

  type(bitset_large) :: set0, set1
  type(bitset_64) :: set2, set3

  if ((set1 == set0) .neqv. .false.) error stop
  if ((set2 == set3) .neqv. .true.) error stop
end program operator_overloading_09
