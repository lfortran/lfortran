subroutine test_falseloc_pack()
    real, allocatable :: bvec(:)
    logical :: lvec(10)
    allocate(bvec(10))
    bvec = 0
    print *, .not. (bvec > 0)
    lvec = .not. (bvec > 0)
    print *, lvec
    if (any(.not. lvec)) error stop
end subroutine

program logical_not_01
    call test_falseloc_pack()
end program
