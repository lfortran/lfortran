! Whole-array expressions inside a Metal device function.
!
! A `pure` function called from an offloaded `do concurrent` is inlined into
! the generated Metal shader as a device function.  Metal has no aggregate
! array assignment, so a whole-array assignment must be expanded into an
! element loop.  That expansion existed only for the kernel body: in a device
! function the right hand side was emitted as a whole and then subscripted,
! which produced invalid Metal such as `r[i] = (x + c)[i]`.
!
! The second `do concurrent` repeats every expression at kernel scope so that
! a regression there is caught as well.  Both loops must offload: the test
! emits two Metal kernels.
module gpu_metal_240_mod
implicit none
integer, parameter :: msz = 4
contains

    pure function f_add(x, c) result(r)
    real, intent(in) :: x(:), c(:)
    real :: r(msz)
    r = x + c
    end function

    pure function f_sub(x, c) result(r)
    real, intent(in) :: x(:), c(:)
    real :: r(msz)
    r = x - c
    end function

    pure function f_lscale(x) result(r)
    real, intent(in) :: x(:)
    real :: r(msz)
    r = 2.0*x
    end function

    pure function f_rscale(x) result(r)
    real, intent(in) :: x(:)
    real :: r(msz)
    r = x*3.0
    end function

    pure function f_neg(x) result(r)
    real, intent(in) :: x(:)
    real :: r(msz)
    r = -x
    end function

    pure function f_compound(x, c) result(r)
    real, intent(in) :: x(:), c(:)
    real :: r(msz)
    r = -2.0*x + c
    end function

    pure function f_iadd(x, c) result(r)
    integer, intent(in) :: x(:), c(:)
    integer :: r(msz)
    r = x + c
    end function

    pure function f_iscale(x) result(r)
    integer, intent(in) :: x(:)
    integer :: r(msz)
    r = 2*x
    end function

end module

program gpu_metal_240
use gpu_metal_240_mod
implicit none
integer, parameter :: n = 5
real :: a(msz,n), b(msz)
real :: o_add(msz,n), o_sub(msz,n), o_ls(msz,n), o_rs(msz,n)
real :: o_neg(msz,n), o_cmp(msz,n)
real :: k_add(msz,n), k_sub(msz,n), k_ls(msz,n), k_rs(msz,n)
real :: k_neg(msz,n), k_cmp(msz,n)
integer :: ia(msz,n), ib(msz), o_int(msz,n), k_int(msz,n)
integer :: o_iscl(msz,n), k_iscl(msz,n)
integer :: i, j

do j = 1, n
    do i = 1, msz
        a(i,j) = real(i) + 10.0*real(j)
        ia(i,j) = i + 10*j
    end do
end do
do i = 1, msz
    b(i) = real(i)
    ib(i) = i
end do

! Every expression evaluated inside an inlined device function.
do concurrent (j = 1:n)
    o_add(:,j) = f_add(a(:,j), b)
    o_sub(:,j) = f_sub(a(:,j), b)
    o_ls(:,j) = f_lscale(a(:,j))
    o_rs(:,j) = f_rscale(a(:,j))
    o_neg(:,j) = f_neg(a(:,j))
    o_cmp(:,j) = f_compound(a(:,j), b)
    o_int(:,j) = f_iadd(ia(:,j), ib)
    o_iscl(:,j) = f_iscale(ia(:,j))
end do

! The same expressions at kernel scope, as a fence.
do concurrent (j = 1:n)
    k_add(:,j) = a(:,j) + b
    k_sub(:,j) = a(:,j) - b
    k_ls(:,j) = 2.0*a(:,j)
    k_rs(:,j) = a(:,j)*3.0
    k_neg(:,j) = -a(:,j)
    k_cmp(:,j) = -2.0*a(:,j) + b
    k_int(:,j) = ia(:,j) + ib
    k_iscl(:,j) = 2*ia(:,j)
end do

do j = 1, n
    do i = 1, msz
        if (abs(o_add(i,j) - (a(i,j) + b(i))) > 1.0e-5) error stop "f_add"
        if (abs(o_sub(i,j) - (a(i,j) - b(i))) > 1.0e-5) error stop "f_sub"
        if (abs(o_ls(i,j) - 2.0*a(i,j)) > 1.0e-5) error stop "f_lscale"
        if (abs(o_rs(i,j) - a(i,j)*3.0) > 1.0e-5) error stop "f_rscale"
        if (abs(o_neg(i,j) + a(i,j)) > 1.0e-5) error stop "f_neg"
        if (abs(o_cmp(i,j) - (-2.0*a(i,j) + b(i))) > 1.0e-4) error stop "f_compound"
        if (o_int(i,j) /= ia(i,j) + ib(i)) error stop "f_iadd"
        if (o_iscl(i,j) /= 2*ia(i,j)) error stop "f_iscale"

        if (abs(k_add(i,j) - (a(i,j) + b(i))) > 1.0e-5) error stop "k_add"
        if (abs(k_sub(i,j) - (a(i,j) - b(i))) > 1.0e-5) error stop "k_sub"
        if (abs(k_ls(i,j) - 2.0*a(i,j)) > 1.0e-5) error stop "k_ls"
        if (abs(k_rs(i,j) - a(i,j)*3.0) > 1.0e-5) error stop "k_rs"
        if (abs(k_neg(i,j) + a(i,j)) > 1.0e-5) error stop "k_neg"
        if (abs(k_cmp(i,j) - (-2.0*a(i,j) + b(i))) > 1.0e-4) error stop "k_cmp"
        if (k_int(i,j) /= ia(i,j) + ib(i)) error stop "k_int"
        if (k_iscl(i,j) /= 2*ia(i,j)) error stop "k_iscl"
    end do
end do

print *, "gpu_metal_240 ok"

end program
