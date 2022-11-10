program print_array

integer :: xarr(3)
xarr(1) = 1
xarr(2) = 2
xarr(3) = 3

call f(3, xarr)

contains

subroutine f(n, x)
    integer :: n, x(n)
    print *, x
end subroutine

end program
