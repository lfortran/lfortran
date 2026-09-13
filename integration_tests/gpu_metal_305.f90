! A per-thread workspace sized from size(c(i)%m_) of a rank-2 component
! whose allocated shapes differ across elements. The launch used to
! stride every thread by the first element's product, so a larger
! element walked into the next slice. Those loops stay on the host.
! size(c(1)%m_) is a compile-time element and still offloads, using
! that element's product rather than extent 1.
program gpu_metal_305
implicit none
type :: leaf_t
    real, allocatable :: m_(:,:)
end type
type(leaf_t) :: c(4)
real :: o(4), p(4)
integer :: i, k
real :: expected

allocate(c(1)%m_(2, 3))
allocate(c(2)%m_(2, 3))
allocate(c(3)%m_(2, 3))
allocate(c(4)%m_(5, 7))

o = -1.0
do concurrent (i = 1:4)
    block
        real, allocatable :: w(:)
        integer :: k
        allocate(w(size(c(i)%m_)))
        do k = 1, size(c(i)%m_)
            w(k) = real(1000 * i + k)
        end do
        o(i) = 0.0
        do k = 1, size(c(i)%m_)
            o(i) = o(i) + w(k)
        end do
        deallocate(w)
    end block
end do

do i = 1, 4
    expected = 0.0
    do k = 1, size(c(i)%m_)
        expected = expected + real(1000 * i + k)
    end do
    if (abs(o(i) - expected) > 1.0e-4) error stop "variant workspace"
end do

p = -1.0
do concurrent (i = 1:4)
    block
        real, allocatable :: w(:)
        integer :: k
        allocate(w(size(c(1)%m_)))
        do k = 1, size(c(1)%m_)
            w(k) = real(100 * i + k)
        end do
        p(i) = 0.0
        do k = 1, size(c(1)%m_)
            p(i) = p(i) + w(k)
        end do
        deallocate(w)
    end block
end do

do i = 1, 4
    expected = 0.0
    do k = 1, 6
        expected = expected + real(100 * i + k)
    end do
    if (abs(p(i) - expected) > 1.0e-4) error stop "const-element workspace"
end do

print *, "PASS"
end program
