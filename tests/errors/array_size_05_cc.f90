program array_size_05_cc
    implicit none
    integer :: a(3)
    integer :: size_a
    integer :: kindvar = 4
    size_a = size(a, kind=kindvar, dim=1)
    size_a = size(a, kind=kindvar)
    print *, "compilation continued despite errors"
end program
