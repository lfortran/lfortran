program array_size_01_cc
    implicit none
    integer :: a(3)
    integer :: size_a
    size_a = size(a, 1, 4, kind=4)
    size_a = size()
    print *, "compilation continued despite errors"
end program
