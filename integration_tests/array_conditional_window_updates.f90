module conditional_window_updates
implicit none
type :: state_t
    integer :: width, channels, kernel_size, filters, stride
    real, allocatable :: kernel(:,:,:), gradient(:,:), dw(:,:,:)
contains
    procedure :: update
end type
contains
pure subroutine update(self, input, gradient, enabled)
    class(state_t), intent(inout) :: self
    real, intent(in) :: input(:,:), gradient(:,:)
    logical, intent(in) :: enabled
    real :: dw(self%filters,self%channels,self%kernel_size)
    real :: gdz(self%filters,self%width)
    integer :: i, k, n, r, iws, iwe
    gdz = gradient
    dw = 0
    self%gradient = 0
    do i = 1, self%width
        iws = 1 + self%stride*(i-1)
        iwe = min(iws + self%kernel_size - 1, size(input,2))
        do k = 1, self%channels
            do n = 1, self%filters
                do r = 1, self%kernel_size
                    dw(n,k,r) = dw(n,k,r) + input(k,iws+r-1)*gdz(n,i)
                end do
                if (enabled) then
                    do r = iws, iwe
                        self%gradient(k,r) = self%gradient(k,r) + gdz(n,i)*self%kernel(n,k,r-iws+1)
                    end do
                end if
            end do
        end do
    end do
    self%dw = self%dw + dw
end subroutine
end module

program array_conditional_window_updates
use conditional_window_updates
implicit none
type(state_t) :: state
integer :: mode
do mode = 0, 1
    call check_case(3, 2, 2, 3, mode == 1)
    call check_case(1, 3, 1, 2, mode == 1)
    call check_case(3, 2, 2, 0, mode == 1)
    call check_case(3, 0, 2, 2, mode == 1)
    call check_case(3, 2, 0, 2, mode == 1)
    call check_case(0, 2, 2, 2, mode == 1)
end do
contains
subroutine check_case(window, filters, channels, width, enabled)
    integer, intent(in) :: window, filters, channels, width
    logical, intent(in) :: enabled
    real, allocatable :: input(:,:), gdz(:,:), expected_gradient(:,:), expected_dw(:,:,:)
    integer :: iw, i, k, n, r, repeat
    iw = max(0, width + window - 1)
    if (allocated(state%kernel)) deallocate(state%kernel)
    if (allocated(state%gradient)) deallocate(state%gradient)
    if (allocated(state%dw)) deallocate(state%dw)
    state%width = width
    state%channels = channels
    state%kernel_size = window
    state%filters = filters
    state%stride = 1
    allocate(state%dw(-1:filters-2,0:channels-1,-1:window-2))
    allocate(state%gradient(channels,-1:iw))
    allocate(input(channels,iw), gdz(filters,width))
    allocate(expected_dw(-1:filters-2,0:channels-1,-1:window-2))
    allocate(expected_gradient(channels,-1:iw))
    if (enabled) then
        allocate(state%kernel(filters,channels,window))
        do r = 1, window
            do k = 1, channels
                do n = 1, filters
                    state%kernel(n,k,r) = real(n + 2*k + 3*r)*0.125
                end do
            end do
        end do
    end if
    state%dw = 0.5
    expected_dw = 0.5
    do repeat = 1, 2
        do i = 1, iw
            do k = 1, channels
                input(k,i) = real(k + 3*i + repeat)*0.03125
            end do
        end do
        do i = 1, width
            do n = 1, filters
                gdz(n,i) = real(n + 2*i + repeat)*0.0625
            end do
        end do
        expected_gradient = 0
        do i = 1, width
            do k = 1, channels
                do n = 1, filters
                    do r = 1, window
                        expected_dw(n-2,k-1,r-2) = expected_dw(n-2,k-1,r-2) + input(k,i+r-1)*gdz(n,i)
                        if (enabled) then
                            expected_gradient(k,i+r-1) = expected_gradient(k,i+r-1) + gdz(n,i)*state%kernel(n,k,r)
                        end if
                    end do
                end do
            end do
        end do
        call state%update(input, gdz, enabled)
        if (any(abs(state%dw - expected_dw) > 1.e-5)) error stop 1
        if (any(abs(state%gradient - expected_gradient) > 1.e-5)) error stop 2
    end do
end subroutine
end program
