module modules_94_m1
    implicit none
    type :: t
        real :: k1 = 0.1
    end type
contains
    subroutine set_k1(me, k1)
        type(t), intent(inout) :: me
        real, intent(in) :: k1
        me%k1 = k1
    end subroutine
end module
