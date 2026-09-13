! A per-thread workspace sized from size(c(i)%m_) of a rank-2
! allocatable component. The sizes buffer holds per-dimension extents,
! so the slice is the product, not extent 1 of the first element.
! Distinct per-dimension extents make a stride of only extent 1 overlap
! adjacent threads.
program gpu_metal_303
implicit none
type :: leaf_t
    real, allocatable :: m_(:,:)
end type
type(leaf_t) :: c(4)
real :: o(4)
integer, parameter :: n1 = 3, n2 = 5
integer :: i, p, q, k
real :: expected

do i = 1, 4
    allocate(c(i)%m_(n1, n2))
    do q = 1, n2
        do p = 1, n1
            c(i)%m_(p, q) = real(100 * i + 10 * q + p)
        end do
    end do
end do

o = -1.0
do concurrent (i = 1:4)
    block
        real, allocatable :: w(:)
        real :: acc
        integer :: k
        allocate(w(size(c(i)%m_)))
        do k = 1, size(c(i)%m_)
            w(k) = real(1000 * i + k)
        end do
        acc = 0.0
        do k = 1, size(c(i)%m_)
            acc = acc + w(k)
        end do
        o(i) = acc
        deallocate(w)
    end block
end do

do i = 1, 4
    expected = 0.0
    do k = 1, n1 * n2
        expected = expected + real(1000 * i + k)
    end do
    if (abs(o(i) - expected) > 1.0e-4) error stop "rank-2 workspace"
end do

print *, "PASS"
end program
