module in_place_sections
    implicit none
    type :: layer
        real, allocatable :: gradient(:,:,:), kernel(:,:,:)
    contains
        procedure :: update
    end type
contains
    subroutine update(self, input, i)
        class(layer), intent(inout) :: self
        real, intent(in) :: input(:,:)
        integer, intent(in) :: i
        self%gradient(i,1:3,1:3) = self%gradient(i,1:3,1:3) + input * self%kernel(i,1:3,1:3)
    end subroutine

    subroutine strided(a, b)
        real, intent(inout) :: a(:,:,:)
        real, intent(in) :: b(:,:)
        a(2,:,:) = a(2,:,:) + b * 0.5
    end subroutine
end module

program check_in_place_sections
    use in_place_sections
    implicit none
    type(layer) :: object
    real :: a(-1:1,0:4,-2:2), b(0:4,-2:2), saved(-1:1,0:4,-2:2)
    real, target :: storage(6)
    real, pointer :: p(:), q(:)
    integer :: i, j
    integer(8) :: lo, hi, step

    do j = -2, 2
        do i = 0, 4
            a(:,i,j) = real(i + 8*j)
            b(i,j) = real(2*i - j)
        end do
    end do
    saved = a
    a(0,4:0:-2,2:-2:-2) = a(0,4:0:-2,2:-2:-2) + b(0:4:2,-2:2:2)*0.5
    do j = 0, 2
        do i = 0, 2
            if (a(0,4-2*i,2-2*j) /= saved(0,4-2*i,2-2*j) + b(2*i,-2+2*j)*0.5) error stop 1
        end do
    end do
    saved = a
    a(0,1:0,:) = a(0,1:0,:) + b(1:0,:)
    if (any(a /= saved)) error stop 2

    a(0,1:4,:) = a(0,1:4,:) + a(0,0:3,:)
    if (any(a(0,1:4,:) /= saved(0,1:4,:) + saved(0,0:3,:))) error stop 3
    saved = a
    lo = 4
    hi = 0
    step = -2
    a(0,lo:hi:step,:) = a(0,lo:hi:step,:) + b(lo:hi:step,:)
    if (any(a(0,4:0:-2,:) /= saved(0,4:0:-2,:) + b(4:0:-2,:))) error stop 4

    a = 1
    b = 4
    call strided(a(1:-1:-1,4:0:-2,2:-2:-2), b(0:4:2,-2:2:2))
    if (any(a(0,4:0:-2,2:-2:-2) /= 3)) error stop 5
    if (any(a(-1,:,:) /= 1) .or. any(a(1,:,:) /= 1)) error stop 6

    allocate(object%gradient(2,3,3), object%kernel(2,3,3))
    object%gradient = 1
    object%kernel = 0.25
    call object%update(b(0:4:2,-2:2:2), 2)
    if (any(object%gradient(2,:,:) /= 2)) error stop 7
    if (any(object%gradient(1,:,:) /= 1)) error stop 8

    storage = [1.,2.,3.,4.,5.,6.]
    p => storage(2:6)
    q => storage(1:5)
    p = p + q
    if (any(storage /= [1.,3.,5.,7.,9.,11.])) error stop 9
end program
