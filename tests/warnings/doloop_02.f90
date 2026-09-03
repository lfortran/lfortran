program doloop_02
    ! A DO loop-control expression only has to be a scalar-int-expr
    ! (F2018 R1125), so a POINTER or ALLOCATABLE integer is valid there
    ! and must not warn. Compare with doloop_01.f90, which does warn.
    integer, pointer :: lo_ptr, hi_ptr, step_ptr
    integer, target :: lo, hi, step
    integer, allocatable :: hi_alloc
    integer :: i

    lo_ptr => lo
    hi_ptr => hi
    step_ptr => step
    allocate(hi_alloc)

    do i = lo_ptr, hi_ptr, step_ptr
        print *, i
    end do

    do i = 1, (hi_ptr)
        print *, i
    end do

    do i = 1, hi_alloc
        print *, i
    end do
end program
