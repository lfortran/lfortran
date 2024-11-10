program array_size_02_cc
    implicit none
    integer :: a(3)
    integer :: size_a
    size_a = size(a, 1, dim=1)
    size_a = size(a, dim = 1, 1)
    print *, "compilation continued despite errors"
end program
