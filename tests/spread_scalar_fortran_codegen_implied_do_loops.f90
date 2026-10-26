program spread_scalar_fortran_codegen_implied_do_loops
    ! After `implied_do_loops` the scalar `source` of `spread` is held in a
    ! one element temporary array; its element is printed as `source`.
    implicit none
    integer :: a, r(3)
    a = 5
    r = spread(a, 1, 3)
    print *, r
end program
