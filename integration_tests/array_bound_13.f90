module array_bound_13_a
    implicit none
    integer, allocatable :: q
    integer, pointer :: p
end module

module array_bound_13_b
    use array_bound_13_a
    implicit none
contains
    subroutine s()
        integer :: d(q), e(p + 1)
        if (size(d) /= 7) error stop
        if (size(e) /= 4) error stop
    end subroutine
end module

program array_bound_13
    use array_bound_13_a
    use array_bound_13_b
    implicit none
    allocate(q, p)
    q = 7
    p = 3
    call s()
    deallocate(q, p)
end program
