module pdt_12
    implicit none

    type recursive_t(k)
        integer, kind :: k = kind(1.0)
        type(recursive_t(k)), allocatable :: next
    end type recursive_t

    interface
        subroutine touch(self)
            type(recursive_t), intent(inout) :: self
        end subroutine touch
    end interface
end module pdt_12
