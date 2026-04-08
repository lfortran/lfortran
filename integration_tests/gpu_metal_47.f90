module m47
! Test: VLA with array broadcast inside block in do concurrent.
! The VLA dimension depends on an intrinsic (maxval) applied to
! a subroutine argument, and the body uses whole-array assignment
! (a = 0.) which becomes an ArrayBroadcast. This exercises the
! gpu_offload pass ordering: VLA dimension pre-computation must
! happen before body Var remapping so that the host-side call
! argument captures the original (caller-scope) references.
implicit none
contains
    subroutine learn(n)
        integer, intent(in) :: n(:)
        integer :: pair
        do concurrent (pair = 1:2)
            block
                real :: a(maxval(n))
                a = 0.
            end block
        end do
    end subroutine
end module m47

program gpu_metal_47
use m47
implicit none
integer :: nodes(2)
nodes = [2, 3]
call learn(nodes)
print *, "ok"
end program gpu_metal_47
