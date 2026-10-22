! COMMON block variables that a pure procedure called in a do concurrent
! loop reads are the storage of the block on the device as well.
module gpu_metal_568_mod
    implicit none
    integer, parameter :: n = 4
contains
    subroutine set_scale(v)
        real, intent(in) :: v
        real :: s
        common /gpu_metal_568_c/ s
        s = v
    end subroutine

    pure real function scalar_get(k)
        integer, intent(in) :: k
        real :: w
        common /gpu_metal_568_a/ w
        scalar_get = w + k
    end function

    ! The second member of the block, after an array.
    pure real function array_get(k)
        integer, intent(in) :: k
        integer :: pad(3)
        real :: w(n)
        common /gpu_metal_568_b/ pad, w
        array_get = w(k) + sum(w) + pad(3)
    end function

    pure real function scaled(k)
        integer, intent(in) :: k
        real :: s
        common /gpu_metal_568_c/ s
        scaled = s * k
    end function
end module

program gpu_metal_568
    use gpu_metal_568_mod
    implicit none
    real :: d, e(n), u(n)
    integer :: p(3)
    common /gpu_metal_568_a/ d
    common /gpu_metal_568_b/ p, e
    integer :: k

    d = 10
    do concurrent (k = 1:n)
        u(k) = scalar_get(k)
    end do
    print *, u
    if (any(u /= [11, 12, 13, 14])) error stop 1

    p = [0, 0, 100]
    e = [1, 2, 3, 4]
    do concurrent (k = 1:n)
        u(k) = array_get(k)
    end do
    print *, u
    if (any(u /= [111, 112, 113, 114])) error stop 2

    ! The caller does not declare the block.
    call set_scale(3.0)
    do concurrent (k = 1:n)
        u(k) = scaled(k)
    end do
    print *, u
    if (any(u /= [3, 6, 9, 12])) error stop 3
end program
