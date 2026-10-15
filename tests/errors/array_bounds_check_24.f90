! The runtime shape check of an explicit-shape dummy whose extent is a
! use-associated variable, when another module has a same-named variable
! (#14505)
module array_bounds_check_24_sa
    integer :: n = 3
end module

module array_bounds_check_24_sb
    integer :: n = 5
end module

module array_bounds_check_24_a
contains
    subroutine fa(x)
        use array_bounds_check_24_sa, only: n
        real :: x(n)
        x = 1
    end subroutine
end module

module array_bounds_check_24_b
contains
    subroutine fb(x)
        use array_bounds_check_24_sb, only: n
        real :: x(n)
        x = 2
    end subroutine
end module

program array_bounds_check_24
    use array_bounds_check_24_a
    use array_bounds_check_24_b
    real :: y(4)
    call fb(y)
end program
