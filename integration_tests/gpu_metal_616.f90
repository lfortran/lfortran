program gpu_metal_616
    ! An offloaded loop calls procedures whose local arrays are sized by
    ! `size(a)` of a dummy that is associated with a function result. Under
    ! --descriptor-index-64 the host computes those sizes as integer(8) from
    ! the result's integer(4) extents (lfortran/lfortran#15893).
    implicit none
    real :: r1(2), r2(2), r3(3), r4(3)
    integer :: i
    do concurrent (i = 1:2)
        r1(i) = f1(e1(3))
    end do
    print *, r1
    if (any(r1 /= 2)) error stop
    do concurrent (i = 1:2)
        r2(i) = f2(e1(3))
    end do
    print *, r2
    if (any(r2 /= 3)) error stop
    do concurrent (i = 1:3)
        r3(i) = f3(e2(2, 3))
    end do
    print *, r3
    if (any(r3 /= 6)) error stop
    do concurrent (i = 1:3)
        r4(i) = f4(e2(2, 3))
    end do
    print *, r4
    if (any(r4 /= 3)) error stop
contains
    pure real function f1(a)
        real, intent(in) :: a(:)
        real :: w(size(a) - 1)
        w = 1
        f1 = sum(w)
    end function

    pure real function f2(a)
        real, intent(in) :: a(:)
        real :: w(size(a))
        w = 1
        f2 = sum(w)
    end function

    pure real function f3(a)
        real, intent(in) :: a(:,:)
        real :: w(size(a))
        w = 1
        f3 = sum(w)
    end function

    pure real function f4(a)
        real, intent(in) :: a(:,:)
        real :: w(size(a, 2))
        w = 1
        f4 = sum(w)
    end function

    pure function e1(n) result(v)
        integer, intent(in) :: n
        real :: v(n)
        v = 1
    end function

    pure function e2(n, m) result(v)
        integer, intent(in) :: n, m
        real :: v(n, m)
        v = 1
    end function
end program
