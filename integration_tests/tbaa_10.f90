! Automatic arrays updated in a loop that also updates allocatable
! components of `self`, in the style of a layer's backward pass: the
! automatic arrays are written element by element between reads and writes
! of the components, and the components are read through their descriptors.
module tbaa_10_m
implicit none
type :: layer_t
    integer :: filters, width, channels, kernel_size
    real, allocatable :: kernel(:,:,:,:)
    real, allocatable :: gradient(:,:)
    real, allocatable :: dw(:,:,:,:)
    real, allocatable :: db(:,:)
end type
contains

subroutine backward(self, input, gradient)
    class(layer_t), intent(inout) :: self
    real, intent(in) :: input(:,:)
    real, intent(in) :: gradient(:,:)
    real :: gdz(self%filters, self%width)
    real :: db_local(self%filters, self%width)
    real :: dw_local(self%filters, self%width, self%channels, self%kernel_size)
    integer :: n, j, k, i
    do j = 1, self%width
        do n = 1, self%filters
            gdz(n, j) = 2*gradient(n, j)
            db_local(n, j) = gdz(n, j)
        end do
    end do
    dw_local = 0
    self%gradient = 0
    do n = 1, self%filters
        do j = 1, self%width
            do k = 1, self%channels
                do i = 1, self%kernel_size
                    dw_local(n, j, k, i) = dw_local(n, j, k, i) &
                        + input(k, j+i-1)*gdz(n, j)
                    self%gradient(k, j+i-1) = self%gradient(k, j+i-1) &
                        + self%kernel(n, j, k, i)*gdz(n, j)
                    gdz(n, j) = gdz(n, j) + dw_local(n, j, k, i)*0.25
                end do
            end do
        end do
    end do
    self%dw = self%dw + dw_local
    self%db = self%db + db_local
end subroutine

end module

program tbaa_10
use tbaa_10_m
implicit none
type(layer_t) :: l
real, allocatable :: input(:,:), gradient(:,:)
real :: gdz(2, 3), dw(2, 3, 2, 2), g(2, 4), gd
integer :: n, j, k, i
l%filters = 2; l%width = 3; l%channels = 2; l%kernel_size = 2
allocate(l%kernel(2, 3, 2, 2), l%gradient(2, 4), l%dw(2, 3, 2, 2), l%db(2, 3))
allocate(input(2, 4), gradient(2, 3))
do i = 1, 2
    do k = 1, 2
        do j = 1, 3
            do n = 1, 2
                l%kernel(n, j, k, i) = n + 2*j + 3*k + 5*i
            end do
        end do
    end do
end do
do j = 1, 4
    input(:, j) = [real(j), real(-j)]
end do
do j = 1, 3
    gradient(:, j) = [0.5*j, 0.25*j]
end do
l%dw = 1
l%db = 1
call backward(l, input, gradient)
! The same computation on plain local arrays.
gdz = 2*gradient
dw = 0
g = 0
do n = 1, 2
    do j = 1, 3
        do k = 1, 2
            do i = 1, 2
                gd = gdz(n, j)
                dw(n, j, k, i) = dw(n, j, k, i) + input(k, j+i-1)*gd
                g(k, j+i-1) = g(k, j+i-1) + l%kernel(n, j, k, i)*gd
                gdz(n, j) = gd + dw(n, j, k, i)*0.25
            end do
        end do
    end do
end do
if (any(abs(l%dw - (1 + dw)) > 1e-5)) error stop 1
if (any(abs(l%db - (1 + 2*gradient)) > 1e-5)) error stop 2
if (any(abs(l%gradient - g) > 1e-4)) error stop 3
print *, sum(l%dw), sum(l%db), sum(l%gradient)
end program
