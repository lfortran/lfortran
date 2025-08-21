program implied_do_loops11
    implicit none
    integer i
    integer, parameter:: nmax = 5, a(nmax) = [1,2,4,8,8], &
        b(nmax-1) = [ ( a(i)-a(i-1), i=2,nmax ) ]
    integer:: c(nmax-1) =  [ ( a(i)-a(i-1), i=2,nmax ) ]
    print "(A,5(1X,I0))", 'a =' ,a
    if (any(a /= [1,2,4,8,8])) error stop
    print "(A,4(1X,I0))", 'b = ',b
    if (any(b /= [1,2,4,0])) error stop
    print "(A,4(1X,I0))", 'c = ',c
    if (any(c /= [1,2,4,0])) error stop
    print "(A,4(1X,I0))", '? = ', ( a(i)-a(i-1), i=2,nmax )
    if (any([ (a(i)-a(i-1), i=2,nmax) ] /= [1,2,4,0])) error stop
end program implied_do_loops11