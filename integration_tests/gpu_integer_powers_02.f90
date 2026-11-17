module gpu_integer_powers_02_m
implicit none
contains
    ! Squares of elements read side by side, as the parts of a sum of squares
    ! of an RMS norm: the elements are read in each factor of the products
    ! (gpu_integer_powers.cpp), so that on an NVIDIA GPU the four of a
    ! statement are still read as one vector of 16 bytes (gpu_wide_loads.cpp).
    pure subroutine squares(p, s)
        real, intent(in) :: p(:)
        real, intent(out) :: s(:)
        integer :: n
        do concurrent (n = 1:size(s))
            block
                real :: a
                integer :: c
                a = 0
                do c = 1, size(p)/4
                    a = a + ((p(4*c-3)**2 + p(4*c-2)**2) + (p(4*c-1)**3 + p(4*c)**4))*n
                end do
                s(n) = a
            end block
        end do
    end subroutine
end module

program gpu_integer_powers_02
! Every value is an integer, so the sums are exact in any order.
use gpu_integer_powers_02_m, only: squares
implicit none
integer, parameter :: k = 512, rows = 3
real :: p(k), s(rows), ref
integer :: i, j
do i = 1, k
    p(i) = real(mod(i, 5) - 2)
end do
call squares(p, s)
do j = 1, rows
    ref = 0
    do i = 1, k, 4
        ref = ref + (p(i)**2 + p(i+1)**2 + p(i+2)**3 + p(i+3)**4)*j
    end do
    if (s(j) /= ref) error stop 1
end do
print *, "ok"
end program
