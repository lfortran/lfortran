subroutine s(n)
    integer, intent(in) :: n
    integer :: a(n, 2)
    integer :: c(product(shape(a)))
    if (size(c) /= 6) error stop
    print *, size(c)
end subroutine
program shape_automatic_bound_01
    call s(3)
end program
