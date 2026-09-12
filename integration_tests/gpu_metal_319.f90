! A `do concurrent` whose body calls a function that sizes its array result
! with a named constant -- `integer, parameter :: end_point = 1` -- is
! offloaded.
!
! The array constructor in the callee needs a per-thread workspace, and the
! host has to size it before it dispatches. A `parameter` reaches the
! backends as a `Var` whose `Variable` carries the value in `m_value`, not
! as a literal node, and the extent pre-flight only recognised literals,
! kernel arguments and singly-assigned locals. So `size(c) - end_point` was
! judged unmeasurable and the loop was declined with
! `workspace-extent-unresolvable`, even though `end_point` is by definition
! known at compile time. Replacing `end_point` with the literal `1` -- and
! nothing else -- used to be the difference between host and device.
!
! Covered here: the constant on its own (`faces_a`), inside a larger
! arithmetic expression (`faces_b`), a `real` parameter folded into an
! integer extent (`faces_c`), and an extent that has no implied-do to carry
! it (`faces_d`).
module gpu_metal_319_mod
implicit none
private
public :: faces_a, faces_b, faces_c, faces_d
integer, parameter :: end_point = 1
integer, parameter :: stride = 2
real, parameter :: half_r = 0.5
contains

    ! The parameter alone in the extent.
    pure function faces_a(c) result(f)
        real, intent(in) :: c(:)
        real, allocatable :: f(:)
        integer :: r
        allocate(f(size(c) - end_point))
        f = [(0.5*(c(r) + c(r+1)), r = 1, size(c) - end_point)]
    end function

    ! The parameter inside a larger arithmetic expression.
    pure function faces_b(c) result(f)
        real, intent(in) :: c(:)
        real, allocatable :: f(:)
        integer :: r
        allocate(f(size(c) - stride*end_point - (stride - 2)))
        f = [(c(r) + 1.0, r = 1, size(c) - stride*end_point - (stride - 2))]
    end function

    ! A `real` parameter folded into an integer extent.
    pure function faces_c(c) result(f)
        real, intent(in) :: c(:)
        real, allocatable :: f(:)
        integer :: r
        allocate(f(size(c) - int(2.0*half_r)))
        f = [(c(r) * 2.0, r = 1, size(c) - int(2.0*half_r))]
    end function

    ! No implied-do: a section inside the constructor.
    pure function faces_d(c) result(f)
        real, intent(in) :: c(:)
        real, allocatable :: f(:)
        allocate(f(size(c) - end_point))
        f = [ c(1:size(c)-end_point) ]
    end function

end module

program gpu_metal_319
use gpu_metal_319_mod, only : faces_a, faces_b, faces_c, faces_d
implicit none
integer, parameter :: nx = 8, ny = 3
real :: c(nx, ny)
real :: fa(nx-1, ny), fb(nx-2, ny), fc(nx-1, ny), fd(nx-1, ny)
integer :: i, j, q

do j = 1, ny
    do i = 1, nx
        c(i,j) = real(i + 10*j)
    end do
end do

fa = 0
fb = 0
fc = 0
fd = 0

do concurrent (q = 1:ny)
    fa(:,q) = faces_a(c(:,q))
end do

do concurrent (q = 1:ny)
    fb(:,q) = faces_b(c(:,q))
end do

do concurrent (q = 1:ny)
    fc(:,q) = faces_c(c(:,q))
end do

do concurrent (q = 1:ny)
    fd(:,q) = faces_d(c(:,q))
end do

do j = 1, ny
    do i = 1, nx - 1
        if (abs(fa(i,j) - (c(i,j) + 0.5)) > 1.0e-5) error stop
        if (abs(fc(i,j) - 2.0*c(i,j)) > 1.0e-5) error stop
        if (abs(fd(i,j) - c(i,j)) > 1.0e-5) error stop
    end do
    do i = 1, nx - 2
        if (abs(fb(i,j) - (c(i,j) + 1.0)) > 1.0e-5) error stop
    end do
end do

print *, fa(:,1)
print *, fb(:,1)
print *, fc(:,1)
print *, fd(:,ny)
end program
