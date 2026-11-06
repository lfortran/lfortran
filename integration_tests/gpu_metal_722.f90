! size, lbound and ubound of kind 8 of an assumed-shape dummy, in a device
! routine called from a do concurrent loop, as operands of max and min
! beside integer(8) values (#16609).
module gpu_metal_722_m
    implicit none
contains
    pure integer(8) function size_max(v) result(r)
        real, intent(in) :: v(:)
        r = max(1_8, size(v, kind=8))
    end function

    pure integer(8) function size_dim_min(v) result(r)
        real, intent(in) :: v(:)
        r = min(100_8, size(v, dim=1, kind=8))
    end function

    pure integer(8) function size_merge(v) result(r)
        real, intent(in) :: v(:)
        r = merge(size(v, kind=8), 0_8, size(v) > 0)
    end function

    pure integer(8) function bounds_max(v) result(r)
        real, intent(in) :: v(:)
        r = max(0_8, lbound(v, 1, kind=8)) + max(1_8, ubound(v, 1, kind=8))
    end function

    pure integer(8) function size_rank2(v) result(r)
        real, intent(in) :: v(:, :)
        r = min(100_8, size(v, kind=8)) + max(1_8, size(v, 2, kind=8))
    end function
end module

program gpu_metal_722
    use gpu_metal_722_m
    implicit none
    real :: x(3, 4), y(3, 2, 4)
    integer(8) :: s1(4), s2(4), s3(4), s4(4), s5(4)
    integer :: k
    x = 1
    y = 1
    do concurrent (k = 1:4)
        s1(k) = k*size_max(x(:, k))
        s2(k) = k*size_dim_min(x(:, k))
        s3(k) = k*size_merge(x(:, k))
        s4(k) = k*bounds_max(x(:, k))
        s5(k) = k*size_rank2(y(:, :, k))
    end do
    if (any(s1 /= [3, 6, 9, 12])) error stop 1
    if (any(s2 /= [3, 6, 9, 12])) error stop 2
    if (any(s3 /= [3, 6, 9, 12])) error stop 3
    if (any(s4 /= [4, 8, 12, 16])) error stop 4
    if (any(s5 /= [8, 16, 24, 32])) error stop 5
    print *, "ok"
end program
