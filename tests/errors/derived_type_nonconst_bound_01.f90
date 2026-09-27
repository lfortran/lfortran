module derived_type_nonconst_bound_01_mod
    implicit none
    integer :: m = 3
    type :: w
        ! `m` is not a named constant, so `m*2` is not constant
        integer :: b(m*2)
    end type
end module

program derived_type_nonconst_bound_01
    use derived_type_nonconst_bound_01_mod
    implicit none
end program
