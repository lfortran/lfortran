program arrays_02
    implicit none
    integer :: a(3)
    integer :: size_a
    integer :: kindvar = 4
    size_a = size(a, kind=kindvar, dim=1)
end
