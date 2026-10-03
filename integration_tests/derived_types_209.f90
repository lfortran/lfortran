module derived_types_209_mod
    implicit none
    type :: pair
        sequence
        integer :: a, b
    end type pair
contains
    integer function pair_sum(p)
        type(pair), intent(in) :: p
        pair_sum = p%a + p%b
    end function pair_sum
end module derived_types_209_mod

program derived_types_209
    use derived_types_209_mod, only: pair_sum
    implicit none
    ! A sequence type of the same name and components is the same type as
    ! the module's. The LLVM backend still gives the two definitions
    ! different types (#13782), so this is not tested with it yet.
    type :: pair
        sequence
        integer :: a, b
    end type pair
    type(pair) :: p
    p%a = 3
    p%b = 4
    if (pair_sum(p) /= 7) error stop
    print *, pair_sum(p)
end program derived_types_209
