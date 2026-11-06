program gpu_derived_component_20
    implicit none
    type metadata
        integer :: tag
        real :: weights(2)
    end type
    type base
        real, allocatable :: v(:)
        integer :: id
    end type
    type, extends(base) :: tensor_t
        type(metadata) :: info
        type(metadata) :: records(2)
        real :: scale
    end type
    type(tensor_t) :: a(2)
    real :: x(2) = [3., 4.]
    integer :: i

    do i = 1, 2
        a(i)%v = [-1., -2.]
        a(i)%id = -1
        a(i)%scale = -1.
    end do
    do concurrent (i = 1:2)
        a(i) = make_tensor(x, i)
    end do
    do i = 1, 2
        if (.not. allocated(a(i)%v)) error stop 1
        if (size(a(i)%v) /= 2) error stop 2
        if (any(a(i)%v /= x)) error stop 3
        if (a(i)%id /= i) error stop 4
        if (a(i)%scale /= real(i)) error stop 5
        if (a(i)%info%tag /= i + 10) error stop 6
        if (any(a(i)%info%weights /= x)) error stop 7
        if (a(i)%records(2)%tag /= i + 20) error stop 8
        if (any(a(i)%records(1)%weights /= x)) error stop 9
    end do
contains
    pure function make_tensor(x, id) result(r)
        real, intent(in) :: x(:)
        integer, intent(in) :: id
        type(tensor_t) :: r
        r%v = x
        r%id = id
        r%scale = real(id)
        r%info%tag = id + 10
        r%info%weights = x
        r%records(1)%tag = id + 20
        r%records(2)%tag = id + 20
        r%records(1)%weights = x
        r%records(2)%weights = x
    end function
end program
