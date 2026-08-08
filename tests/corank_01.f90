program corank_01
    implicit none

    integer :: a[*]
    integer :: b[2, *]
    integer :: c[2, 3, *]

    print *, corank(a)
    print *, corank(b)
    print *, corank(c)

end program corank_01