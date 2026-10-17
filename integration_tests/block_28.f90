program block_28
    ! The module block_28_a, in block_28_a.f90, depends on the module
    ! block_28_b, in block_28_b.f90, only through a USE statement in the
    ! interface bodies of a BLOCK construct.
    use block_28_a, only: twice
    implicit none
    if (twice(5) /= 10) error stop
    print *, twice(5)
end program block_28
