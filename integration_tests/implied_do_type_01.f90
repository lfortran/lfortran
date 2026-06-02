program test_implied_do_type
    implicit none
    integer, parameter :: N = 8
    character(len=*), parameter :: row_fmt = '(9(I6,:,1X))'
    integer, parameter :: c(0:N, 0:N) = &
        reshape([([(product([(jj, integer :: jj=n-k+1,n)]) / &
                   product([(jj, integer :: jj=1,k)]),  &
                   integer :: n=0,N)], integer :: k=0,N)], [N+1, N+1], order=[2,1])
    write(*,row_fmt) c
end program test_implied_do_type