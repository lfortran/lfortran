module gpu_metal_483_m
! A fixed-size local array that every iteration of a DO CONCURRENT loop
! defines whole before it references it is private to the iteration
! (Fortran 2018, 11.1.7.5).  On a GPU it must become a per-thread local:
! one buffer that every thread writes and reads is a race, and with many
! threads in flight the last writer wins.  The loops are long enough for
! a real GPU to run them in parallel.
implicit none
contains

    ! The definition sits inside an ASSOCIATE construct.
    subroutine assoc_whole(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j
        real :: t(4)
        do concurrent (j = 1:n)
            associate (e => 10.0 * x(j))
                t = [x(j), e, e + 1.0, e + 2.0]
                g(:,j) = t
            end associate
        end do
    end subroutine

    ! Nested ASSOCIATE constructs, the array defined by a DO loop.
    subroutine assoc_nested(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j, k
        real :: t(4)
        do concurrent (j = 1:n)
            associate (a => x(j))
                associate (b => 2.0 * a)
                    do k = 1, 4
                        t(k) = b + real(k)
                    end do
                    g(:,j) = t(4:1:-1)
                end associate
            end associate
        end do
    end subroutine

    ! An explicit LOCAL locality specifier.
    subroutine local_spec(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j
        real :: t(4)
        do concurrent (j = 1:n) local(t)
            t = [x(j), 3.0 * x(j), 3.0 * x(j) + 1.0, 3.0 * x(j) + 2.0]
            g(:,j) = t
        end do
    end subroutine

    ! A constructor outside any ASSOCIATE: its lowering fills a
    ! temporary of the procedure element by element through a counter.
    subroutine plain(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j
        real :: t(4)
        do concurrent (j = 1:n)
            t = [x(j), 4.0 * x(j), 4.0 * x(j) + 1.0, 4.0 * x(j) + 2.0]
            g(:,j) = t
        end do
    end subroutine

    ! Every element defined one by one, later ones reading earlier ones.
    subroutine elements(x, g, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        integer :: j
        real :: t(2,2)
        do concurrent (j = 1:n)
            t(1,1) = x(j)
            t(2,1) = 5.0 * x(j)
            t(1,2) = t(2,1) + 1.0
            t(2,2) = t(1,2) + 1.0
            g(1:2,j) = t(:,1)
            g(3:4,j) = t(:,2)
        end do
    end subroutine

    ! LOCAL locality of an array and of a scalar that are dummy
    ! arguments.
    subroutine local_dummies(x, g, n, t, s)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        real, intent(out) :: g(4,n)
        real, intent(inout) :: t(4), s
        integer :: j
        do concurrent (j = 1:n) local(t, s)
            s = 6.0 * x(j)
            t = [x(j), s, s + 1.0, s + 2.0]
            g(:,j) = t
        end do
    end subroutine

end module

program gpu_metal_483
use gpu_metal_483_m
implicit none
integer, parameter :: n = 100000
real, allocatable :: x(:), g(:,:)
real :: tout(4), sout
integer :: j, k

allocate(x(n), g(4,n))
do j = 1, n
    x(j) = real(mod(j, 997))
end do

g = 0.0
call assoc_whole(x, g, n)
do j = 1, n
    if (abs(g(1,j) - x(j)) > 1.0e-3) error stop "assoc_whole: element 1"
    if (abs(g(2,j) - 10.0 * x(j)) > 1.0e-3) error stop "assoc_whole: element 2"
    if (abs(g(3,j) - (10.0 * x(j) + 1.0)) > 1.0e-3) error stop "assoc_whole: element 3"
    if (abs(g(4,j) - (10.0 * x(j) + 2.0)) > 1.0e-3) error stop "assoc_whole: element 4"
end do

g = 0.0
call assoc_nested(x, g, n)
do j = 1, n
    do k = 1, 4
        if (abs(g(k,j) - (2.0 * x(j) + real(5 - k))) > 1.0e-3) error stop "assoc_nested"
    end do
end do

g = 0.0
call local_spec(x, g, n)
do j = 1, n
    if (abs(g(1,j) - x(j)) > 1.0e-3) error stop "local: element 1"
    if (abs(g(2,j) - 3.0 * x(j)) > 1.0e-3) error stop "local: element 2"
    if (abs(g(3,j) - (3.0 * x(j) + 1.0)) > 1.0e-3) error stop "local: element 3"
    if (abs(g(4,j) - (3.0 * x(j) + 2.0)) > 1.0e-3) error stop "local: element 4"
end do

g = 0.0
call plain(x, g, n)
do j = 1, n
    do k = 1, 4
        if (abs(g(k,j) - merge(x(j), 4.0 * x(j) + real(k - 2), k == 1)) > 1.0e-3) error stop "plain"
    end do
end do

g = 0.0
call elements(x, g, n)
do j = 1, n
    do k = 1, 4
        if (abs(g(k,j) - merge(x(j), 5.0 * x(j) + real(k - 2), k == 1)) > 1.0e-3) error stop "elements"
    end do
end do

g = 0.0
tout = -1.0
sout = -2.0
call local_dummies(x, g, n, tout, sout)
do j = 1, n
    do k = 1, 4
        if (abs(g(k,j) - merge(x(j), 6.0 * x(j) + real(k - 2), k == 1)) > 1.0e-3) error stop "local_dummies"
    end do
end do

print *, "ok"
end program
