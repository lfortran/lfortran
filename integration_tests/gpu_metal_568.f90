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

    ! Only the variables up to the last one read place it; the character
    ! and pointer variables before it are not read.
    pure real function after_others(k)
        integer, intent(in) :: k
        character(len=3) :: tag
        real, pointer :: q
        real :: v(2)
        common /gpu_metal_568_d/ tag, q, v
        after_others = v(mod(k, 2) + 1) * k
    end function

    ! A run-time sized local, which device code is given a workspace for.
    pure real function with_workspace(k)
        integer, intent(in) :: k
        real :: w(n)
        common /gpu_metal_568_e/ w
        real :: t(k)
        t = w(k)
        with_workspace = sum(t)
    end function

    ! Reads the block through a procedure it calls.
    pure real function through_call(k)
        integer, intent(in) :: k
        through_call = 2 * scalar_get(k)
    end function

    ! Reads the block scalar_get reads, through a view of its own.
    pure real function scalar_twice(k)
        integer, intent(in) :: k
        real :: w2
        common /gpu_metal_568_a/ w2
        scalar_twice = 2 * w2 * k
    end function

    pure real function blank_get(k)
        integer, intent(in) :: k
        real :: b(n)
        common b
        blank_get = b(k) + 1
    end function

    ! Laid out as the loop that defines the block lays it out.
    pure real function shifted(k)
        integer, intent(in) :: k
        real :: g(7)
        common /gpu_metal_568_f/ g
        shifted = g(k + 3)
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
    character(len=3) :: tag
    real, pointer :: q
    real :: v(2), x(n)
    common /gpu_metal_568_d/ tag, q, v
    common /gpu_metal_568_e/ x
    real :: y(n), f(7)
    common y
    common /gpu_metal_568_f/ f
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

    tag = "abc"
    q => null()
    v = [10, 20]
    do concurrent (k = 1:n)
        u(k) = after_others(k)
    end do
    print *, u
    if (any(u /= [20, 20, 60, 40])) error stop 3

    x = [1, 2, 3, 4]
    do concurrent (k = 1:n)
        u(k) = with_workspace(k)
    end do
    print *, u
    if (any(u /= [1, 4, 9, 16])) error stop 4

    ! The caller does not declare the block.
    call set_scale(3.0)
    do concurrent (k = 1:n)
        u(k) = scaled(k)
    end do
    print *, u
    if (any(u /= [3, 6, 9, 12])) error stop 5

    do concurrent (k = 1:n)
        u(k) = through_call(k)
    end do
    print *, u
    if (any(u /= [22, 24, 26, 28])) error stop 6

    ! Two procedures read the same block.
    do concurrent (k = 1:n)
        u(k) = scalar_get(k) + scalar_twice(k)
    end do
    print *, u
    if (any(u /= [31, 52, 73, 94])) error stop 7

    y = [5, 6, 7, 8]
    do concurrent (k = 1:n)
        u(k) = blank_get(k)
    end do
    print *, u
    if (any(u /= [6, 7, 8, 9])) error stop 8

    f = 0
    do concurrent (k = 1:n)
        f(k + 3) = k
        u(k) = shifted(k)
    end do
    print *, u
    if (any(u /= [1, 2, 3, 4])) error stop 9
end program
