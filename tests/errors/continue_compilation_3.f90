program continue_compilation_3
implicit none

    ! declarations
    integer :: k = 3
    integer :: i, iarx(3)

    ! data_implied_do1
    data(iarx(i), i=1, k) / 1, 2, 3 /
    print *, "First error skipped"

    ! data_implied_do2
    data(iarx(i), i=1, 3, k) / 1, 2, 3 /
    print *, "Second error skipped"

    ! data_implied_do3
    data(iarx(i), i=k, 3) / 1, 2, 3 /
    print *, "Third error skipped"
    
end program