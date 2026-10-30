program arrays_161
    implicit none
    integer :: n, i, j, a(3), b(1)
    n = 3
    i = 19
    j = 23
    a = [(i, i=1,size([(j,j=1,n)]))]
    if (any(a /= [1,2,3])) error stop 1
    if (i /= 19 .or. j /= 23) error stop 2
    b = [sum([(i,i=1,size([(j,j=1,n)]))])]
    if (b(1) /= 6) error stop 3
    if (i /= 19 .or. j /= 23) error stop 4
end program
