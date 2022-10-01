program doloop_08
    implicit none
    integer :: n=10, digit, sum=0
    do
        if(n==0) exit
        digit=mod(n,10)
        sum=sum + digit
        n=n/10
    end do
    print *, sum
end program doloop_08