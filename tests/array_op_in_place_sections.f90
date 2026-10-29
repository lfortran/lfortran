module in_place_sections_ir
    type :: layer
        real, allocatable :: gradient(:,:,:), kernel(:,:,:)
    end type
contains
    subroutine update(self, work, input, n, k)
        class(layer), intent(inout) :: self
        real, intent(inout) :: work(:,:,:,:)
        real, intent(in) :: input(:,:)
        integer, intent(in) :: n, k
        work(n,k,:,:) = work(n,k,:,:) + input * 0.5
        self%gradient(k,1:3,1:3) = self%gradient(k,1:3,1:3) + input*self%kernel(k,1:3,1:3)
    end subroutine
end module
