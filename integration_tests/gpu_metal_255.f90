! A device function that has to be spliced into the kernel body -- its
! array constructor is materialized into a temporary sized from an
! assumed-shape dummy, and Metal has neither variable-length arrays nor a
! heap -- used to be refused whenever its symbol table held anything but
! plain Variables.
!
! Reaching a component of a derived type declared in a module imports an
! ExternalSymbol for the type and one for each component touched, and
! those land in the referencing procedure's own symbol table. They only
! name entities owned by that module and keep resolving through it
! wherever the cloned body ends up, so they need no re-homing; refusing
! them only meant the enclosing `do concurrent` was declined and silently
! ran on the host.
!
! The decline is silent and the values are correct either way, so the
! kernel count is what this test is about:
! `--gpu=metal --show-gpu-kernel-source` reports 2 kernels here, one per
! loop below. Before the fix it reported 0.
module gpu_metal_255_types
    implicit none
    type :: op_t
        real :: head_
        real :: tail_
    end type op_t
end module gpu_metal_255_types

program gpu_metal_255
    use gpu_metal_255_types, only: op_t
    implicit none
    integer, parameter :: n = 4
    real :: a(8,n), b(8,n)
    real :: v(2), m(2,2)
    type(op_t) :: op
    integer :: j, k
    v = [10.0, 20.0]
    m = reshape([1.0, 0.0, 0.0, 1.0], [2,2])
    k = 3
    op%head_ = 5.0
    op%tail_ = 2.0
    a = 0.0
    b = 0.0

    do concurrent (j = 1:n)
        a(:,j) = f(m, v, k, op)
    end do

    do concurrent (j = 1:n)
        b(:,j) = g(m, v, k, op)
    end do

    do j = 1, n
        if (abs(a(1,j) - 5.0) > 1.0e-6) error stop "leading component wrong"
        if (abs(a(2,j) - 10.0) > 1.0e-6) error stop "matmul segment wrong"
        if (abs(a(3,j) - 20.0) > 1.0e-6) error stop "matmul segment wrong"
        if (abs(a(4,j) - 7.0) > 1.0e-6) error stop "local segment wrong"
        if (abs(a(5,j) - 7.0) > 1.0e-6) error stop "local segment wrong"
        if (abs(a(6,j) - 7.0) > 1.0e-6) error stop "local segment truncated"
        if (abs(a(7,j) - 2.0) > 1.0e-6) error stop "trailing component wrong"
        if (abs(a(8,j)) > 1.0e-6) error stop "trailing scalar wrong"
        if (abs(b(1,j) - 7.0) > 1.0e-6) error stop "g leading wrong"
        if (abs(b(2,j) - 10.0) > 1.0e-6) error stop "g matmul segment wrong"
        if (abs(b(3,j) - 20.0) > 1.0e-6) error stop "g matmul segment wrong"
        if (abs(b(4,j) - 10.0) > 1.0e-6) error stop "g local segment wrong"
        if (abs(b(5,j) - 10.0) > 1.0e-6) error stop "g local segment wrong"
        if (abs(b(6,j) - 10.0) > 1.0e-6) error stop "g local segment truncated"
        if (abs(b(7,j)) > 1.0e-6) error stop "g trailing wrong"
        if (abs(b(8,j)) > 1.0e-6) error stop "g trailing wrong"
    end do

    print *, a(:,1)
    print *, b(:,1)
    print *, "ok"

contains

    ! `matmul(mm, x)` is array-valued, so the constructor is materialized
    ! into a temporary; `mm` and `x` are assumed shape, so its extent is
    ! a run-time parameter of the device function. `op_t`, `head_` and
    ! `tail_` are ExternalSymbols in this function's own symbol table.
    pure function f(mm, x, kk, o) result(r)
    real, intent(in) :: mm(:,:), x(:)
    integer, intent(in) :: kk
    type(op_t), intent(in) :: o
    real :: r(8)
    real :: w(3)
    w = real(kk) + 4.0
    r = [o%head_, matmul(mm, x), w, o%tail_, 0.0]
    end function f

    ! The same, with the components used in arithmetic rather than
    ! passed straight through.
    pure function g(mm, x, kk, o) result(r)
    real, intent(in) :: mm(:,:), x(:)
    integer, intent(in) :: kk
    type(op_t), intent(in) :: o
    real :: r(8)
    real :: w(3)
    w = real(kk) + o%head_ + o%tail_
    r = [o%head_ + o%tail_, matmul(mm, x), w, 0.0, 0.0]
    end function g

end program gpu_metal_255
