program main
    integer :: a(2)
    integer :: temp(5)
    integer :: i = 1
    a = temp(i+1:i+1)
    print *, size(a)
end program
