! Fortran code after applying the pass: simplifier
program pointer_to_data_array_01
implicit none
integer(4) :: k1
real(4), dimension(2) :: array_constant
integer(4), dimension(2) :: ac1
real(4), dimension(4) :: array_reshape
real(4), dimension(2, 2) :: a
real(4) :: d
real(4), dimension(2) :: g
array_reshape = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00, 4.00000000e+00]
ac1 = [2, 2]
a = reshape(array_reshape, ac1)
k1 = lbound(array_constant, 1)
array_constant(k1) = 5.00000000e+00
k1 = k1 + 1
array_constant(k1) = 6.00000000e+00
k1 = k1 + 1
g = array_constant
d = trstlp(a, g)
print *, d

contains

real(4) function trstlp(a, g) result(d)
    integer(4) :: i1
    integer(4) :: t2
    integer(4) :: t1
    integer(4) :: t3
    real(4), dimension(:), allocatable :: ac
    integer(4), dimension(2) :: ac2
    real(4), dimension(:, :) :: a
    real(4), dimension(:) :: g
    real(4), dimension(size(a) + size(g)) :: array_reshape
    real(4), dimension(size(a, 1), size(a, 2) + 1) :: a_aug
    integer(4) :: m
    integer(4) :: n
    print *, "A = ", a
    print *, "g = ", g
    ! deallocate(ac)
    allocate(ac(size(a) + size(g)))
    i1 = lbound(ac, 1)
    do t3 = lbound(a, 2), ubound(a, 2)
        do t2 = lbound(a, 1), ubound(a, 1)
            ac(i1) = a(t2, t3)
            i1 = i1 + 1
        end do
    end do
    do t1 = lbound(g, 1), ubound(g, 1)
        ac(i1) = g(t1)
        i1 = i1 + 1
    end do
    print *, "[A, g] = ", ac
    m = size(a, 2)
    n = size(a, 1)
    array_reshape = [a, g]
    ac2 = [n, m + 1]
    print *, "array_reshape = ", array_reshape
    a_aug = reshape(array_reshape, ac2)
    print *, "A_aug = ", a_aug
    d = Sum(a_aug)
    print *, d
end function trstlp

end program pointer_to_data_array_01

