program gpu_metal_595
    implicit none
    real, allocatable :: a(:)
    real :: b(2)
    integer :: j
    allocate(a(2))
    a = 1.0
    do concurrent (j = 1:1)
        b = shifted(a(:))
    end do
    if (any(b /= 2.0)) error stop
contains
    pure function shifted(arr) result(r)
        real, intent(in) :: arr(:)
        real :: r(size(arr)), t(size(arr)-1)
        integer :: i
        ! This section copy forces gathering the actual when the callee is spliced.
        t = arr(2:)
        do i = 1, size(arr)-1
            r(i) = dot_product([1.0,1.0], arr(i:i+1))
        end do
        r(size(arr)) = 2.0
    end function
end program
