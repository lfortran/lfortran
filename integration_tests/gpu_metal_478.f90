module gpu_metal_478_m
implicit none
contains
    subroutine changed_pointer_bound(m)
        integer, pointer, intent(in) :: m
        integer, pointer :: p
        real :: t(m), y(4)
        integer :: j, k
        t = 7
        p => m
        p = 2
        do concurrent (j = 1:4)
            do k = 1, m
                t(k) = j
            end do
            y(j) = t(4)
        end do
        if (any(y /= 7)) error stop 1
    end subroutine
end module
program gpu_metal_478
use gpu_metal_478_m
implicit none
integer, target :: extent
integer, pointer :: m
extent = 4
m => extent
call changed_pointer_bound(m)
if (extent /= 2) error stop 2
print *, "ok"
end program
