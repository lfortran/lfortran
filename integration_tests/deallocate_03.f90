program deallocate_03
    implicit none
    integer, allocatable :: a(:)

    ! No allocate(a) here
    deallocate(a)

    print *, "This line should not execute"
end program deallocate_03