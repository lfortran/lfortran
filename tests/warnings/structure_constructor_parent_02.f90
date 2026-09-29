module structure_constructor_parent_02_a
    implicit none
    type :: base_t
        integer :: a1 = 1
    end type
end module

module structure_constructor_parent_02_b
    use structure_constructor_parent_02_a, only: p_t => base_t
    implicit none
    type, extends(p_t) :: der_t
        integer :: b1 = 2
    end type
end module

module structure_constructor_parent_02_c
    use structure_constructor_parent_02_b, only: p_t => der_t
    implicit none
    type, extends(p_t) :: der3_t
        integer :: c1 = 3
    end type
end module

program structure_constructor_parent_02
    use structure_constructor_parent_02_a
    use structure_constructor_parent_02_b
    use structure_constructor_parent_02_c
    implicit none
    type, extends(der_t) :: der2_t
        integer :: c1 = 3
    end type
    type(der_t) :: d
    type(der2_t) :: d2
    type(der3_t) :: d3
    ! The parent component is named `p_t`, as `der_t` knows its parent.
    d = der_t(base_t(8), 9)
    print *, d%a1, d%b1
    ! The same inherited parent component.
    d2 = der2_t(base_t(8), 9, 10)
    print *, d2%a1, d2%b1, d2%c1
    ! The parent component `p_t` of `der3_t` hides the inherited one of the
    ! same name, so no keyword gives a `base_t` value here.
    d3 = der3_t(base_t(8), 9, 10)
    print *, d3%a1, d3%b1, d3%c1
end program
