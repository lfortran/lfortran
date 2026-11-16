program coarrays_08
    implicit none
    integer :: a(5)[*], b(5)
    character(len=3) :: s(2)[*], c(2)

    a = 0
    b = [1, 2, 3, 4, 5]
    s = ['abc', 'def']
    if (this_image() == 1) a(:)[1] = b

    sync all

    if (any(a(:)[1] /= b)) error stop
    c = s(:)[1]
    if (any(c /= ['abc', 'def'])) error stop
end program
