! The runtime shape check of an explicit-shape dummy whose extent is a
! use-associated variable also applies to a call through a procedure pointer
module array_bounds_check_23_sizes
    integer :: n = 2
end module

module array_bounds_check_23_m
    abstract interface
        subroutine cb(a)
            use array_bounds_check_23_sizes, only: n
            real :: a(n)
        end subroutine
    end interface
contains
    subroutine g(a)
        use array_bounds_check_23_sizes, only: n
        real :: a(n)
        a = 9
    end subroutine
end module

program array_bounds_check_23
    use array_bounds_check_23_m
    procedure(cb), pointer :: q
    real :: a(1)
    q => g
    call q(a)
end program
