submodule(submodule_50_mod) submodule_50_sub
    implicit none
contains
    module subroutine setup_impl(this, lengths)
        class(container_type), intent(inout) :: this
        integer, dimension(this%rank), intent(in) :: lengths
        integer :: i
        do i = 1, this%rank
            if (lengths(i) < 0) error stop
        end do
    end subroutine
end submodule
