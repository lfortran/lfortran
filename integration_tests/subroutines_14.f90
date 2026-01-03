subroutine check_logical(expression)
    logical, intent(in) :: expression
    print *, expression
    if (.not. expression) error stop
    end subroutine check_logical
    
    subroutine test_trueloc_empty()
    real, allocatable :: avec(:), bvec(:)
    allocate(avec(10))
    allocate(bvec(10))
    
    avec = 0
    bvec = 0
    call check_logical(all(bvec == avec))
end subroutine test_trueloc_empty
    
program subroutines_14
    call test_trueloc_empty()
end program
