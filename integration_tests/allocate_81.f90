! allocate with MOLD= from an allocatable component of a derived type (#5400)
module allocate_81_mod
    type :: my_type
        real, allocatable :: kernel(:)
        real, allocatable :: dw(:)
    end type
end module allocate_81_mod

program allocate_81
    use allocate_81_mod
    implicit none
    type(my_type) :: self
    real, allocatable :: array1(:), array2(:)
    integer :: i, n

    allocate(self % kernel(1:10))
    self % kernel = 1.0

    allocate(self % dw, mold=self % kernel)

    print *, "Shape of kernel:", shape(self % kernel)
    print *, "Shape of dw:", shape(self % dw)
    if (.not. allocated(self % dw)) error stop
    if (size(self % dw) /= 10) error stop
    if (any(shape(self % dw) /= [10])) error stop
    if (lbound(self % dw, 1) /= 1 .or. ubound(self % dw, 1) /= 10) error stop

    ! source= with a runtime size (from the issue discussion)
    n = 4
    allocate(array1(1:n))
    array1 = [(i * 1.0, i = 1, n)]
    allocate(array2, source=array1)
    if (size(array1) /= 4) error stop
    if (size(array2) /= 4) error stop
    if (any(array2 /= [1.0, 2.0, 3.0, 4.0])) error stop
    deallocate(array1, array2)
end program allocate_81
