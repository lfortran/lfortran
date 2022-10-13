program arrays_01
    implicit none
    integer :: a(3)
    real :: size_a
    print *, transfer(a, 1, size_a)
end
