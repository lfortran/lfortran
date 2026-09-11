! Two kernels in one translation unit, the first with an array-of-struct
! argument. The device functions of the second kernel are emitted before
! that kernel binds its own arguments, so the first kernel's per-element
! sizes buffer was still registered and got spelled into the second
! kernel's device function, which then referenced `__sizes_t_v` and the
! first kernel's loop index -- neither of them in scope there.
module gpu_metal_333_mod
    implicit none
    type :: tensor_t
        real, allocatable :: v(:)
    end type
contains
    pure real function inner(p) result(r)
        type(tensor_t), intent(in) :: p
        integer :: k
        r = 0.0
        do k = 1, size(p%v)
            r = r + p%v(k)
        end do
    end function

    pure real function helper(p) result(r)
        type(tensor_t), intent(in) :: p
        r = inner(p) * 2.0
    end function
end module

program gpu_metal_333
    use gpu_metal_333_mod
    implicit none
    integer :: i
    type(tensor_t) :: t(3)
    type(tensor_t) :: w
    type(tensor_t) :: p
    real :: r(3), r2(3)

    allocate(t(1)%v(2), t(2)%v(2), t(3)%v(2))
    t(1)%v = [1.0, 2.0]
    t(2)%v = [3.0, 4.0]
    t(3)%v = [6.0, 7.0]
    allocate(w%v(2))
    w%v = [1.0, 2.0]

    do concurrent (i = 1:3)
        p = t(i)
        r(i) = inner(p)
    end do

    do concurrent (i = 1:3)
        r2(i) = helper(w)
    end do

    if (abs(r(1) - 3.0) > 1e-5) error stop
    if (abs(r(2) - 7.0) > 1e-5) error stop
    if (abs(r(3) - 13.0) > 1e-5) error stop
    do i = 1, 3
        if (abs(r2(i) - 6.0) > 1e-5) error stop
    end do
    print *, "PASS"
end program
