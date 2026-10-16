! A run-time sized temporary for an overlapping array assignment that is not
! at the top level of a `do concurrent` body, or whose extent changes from one
! iteration to the next.
!
! `delta(1:n(l),l) = ... delta(1:n(l+1),l+1) ...` reads storage of the array
! it writes, so it needs a temporary, and it has to be private to the
! iteration. It is given a BLOCK of its own around the assignment, wherever
! the assignment is, and the BLOCK local becomes a per-thread workspace. The
! host sizes that workspace before the launch, so an extent that changes with
! the iteration (`n(l)` in a loop over `l`), or one the host cannot evaluate
! (`f(m)`), is sized as the whole dimension of the array instead.
module gpu_metal_387_m
implicit none

type :: net_t
    integer, allocatable :: n(:)
    real, allocatable :: w(:,:,:), b(:,:), c(:,:,:)
end type

contains

    pure integer function f(i)
        integer, intent(in) :: i
        f = i + 1
    end function

    ! Inside a BLOCK, a serial loop and an ASSOCIATE, through associate
    ! names for components.
    subroutine backprop(net, m, r)
        type(net_t), intent(in) :: net
        integer, intent(in) :: m
        real, intent(out) :: r(:,:,:)
        integer :: k, l
        associate(w => net%w, b => net%b, n => net%n)
            do concurrent (k = 1:m)
                block
                    real :: delta(size(b,1),size(b,2))
                    delta = 0.0
                    delta(1:n(3),3) = real(k)
                    associate(top => ubound(n,1) - 1)
                        do l = top, 1, -1
                            delta(1:n(l),l) = matmul(transpose( &
                                w(1:n(l+1),1:n(l),l+1)), &
                                delta(1:n(l+1),l+1)) + b(1:n(l),l)
                        end do
                    end associate
                    r(:,:,k) = delta
                end block
            end do
        end associate
    end subroutine

    ! A component as the base, with a reversed section in a serial loop.
    subroutine reverse(net, m)
        type(net_t), intent(inout) :: net
        integer, intent(in) :: m
        integer :: k, l
        do concurrent (k = 1:m)
            do l = 1, 2
                net%c(1:net%n(l),l,k) = net%c(net%n(l):1:-1,l+1,k)
            end do
        end do
    end subroutine

    ! An extent the host cannot evaluate: a function call.
    subroutine flip(a, m)
        real, intent(inout) :: a(:,:)
        integer, intent(in) :: m
        integer :: c
        do concurrent (c = 1:size(a,2))
            a(1:f(m),c) = a(f(m):1:-1,c)
        end do
    end subroutine

    ! An assumed-shape dummy in a serial loop.
    subroutine shift(w)
        real, intent(inout) :: w(:,:)
        integer :: i, k
        do concurrent (i = 1:size(w,2))
            do k = 1, 1
                w(:,i) = w(:,i) + w(1,i)
            end do
        end do
    end subroutine

end module

program gpu_metal_387
use gpu_metal_387_m
implicit none
type(net_t) :: net
real :: r(3,3,4), a(3,4), w(3,4)
integer :: i, j, k

allocate(net%n(0:3))
net%n = [2, 3, 2, 3]
allocate(net%w(3,3,3), net%b(3,3))
net%w = 1.0
net%b = 0.5
call backprop(net, 4, r)
do k = 1, 4
    if (any(abs(r(1:3,3,k) - k) > 1.0e-5)) error stop 1
    if (any(abs(r(1:2,2,k) - (3*k + 0.5)) > 1.0e-5)) error stop 2
    if (abs(r(3,2,k)) > 1.0e-5) error stop 3
    if (any(abs(r(1:3,1,k) - (2*(3*k + 0.5) + 0.5)) > 1.0e-5)) error stop 4
end do

allocate(net%c(3,3,4))
do k = 1, 4
    net%c(:,1,k) = 0.0
    net%c(:,2,k) = [1, 2, 3]*k
    net%c(:,3,k) = [4, 5, 6]*k
end do
call reverse(net, 4)
do k = 1, 4
    if (any(abs(net%c(:,1,k) - [3, 2, 1]*k) > 1.0e-5)) error stop 5
    if (any(abs(net%c(:,2,k) - [5, 4, 3]*k) > 1.0e-5)) error stop 6
end do

do j = 1, 4
    a(:,j) = [1, 2, 3]*j
end do
call flip(a, 2)
do j = 1, 4
    if (any(abs(a(:,j) - [3, 2, 1]*j) > 1.0e-5)) error stop 7
end do

do j = 1, 4
    do i = 1, 3
        w(i,j) = real(i + j)
    end do
end do
call shift(w)
do j = 1, 4
    do i = 1, 3
        if (abs(w(i,j) - real(2*j + i + 1)) > 1.0e-5) error stop 8
    end do
end do
print *, "ok"
end program
