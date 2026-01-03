program arrays_01
    implicit none
    integer :: a(3)
    integer :: size_a
    size_a = size(a, 1, 4, kind=4)
end
