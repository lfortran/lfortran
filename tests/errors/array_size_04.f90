program array_size_04
    implicit none
    integer :: a(3)
    integer :: size_a
    size_a = size(a, kind1=4, dim1=1)
end
