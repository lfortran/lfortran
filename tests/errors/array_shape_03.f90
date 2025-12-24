program main
    integer :: a(2) = [2, 3]
    integer :: temp(2)
    integer :: i0 = 1
    a = temp(i0:i0)
    print *, size(a)
end program
