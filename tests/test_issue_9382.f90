program test_stdin_9382
    implicit none
    integer :: m, n, i
    do i = 1, 2
        read(*,*) m
        read(*,*) n
        print *, 'Loop', i, ': m=', m, ' n=', n
    end do
end program
