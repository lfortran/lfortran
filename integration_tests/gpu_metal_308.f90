! A workspace sized from size(c(2)%m_), a compile-time element that is
! not the first. The host used to allocate from c(1)'s product while the
! device strided by c(2)'s, so unequal shapes overlapped. Both sides
! must use element 2.
program gpu_metal_308
implicit none
type :: leaf_t
    real, allocatable :: m_(:,:)
end type
type(leaf_t) :: c(2)
real :: o(4)
integer :: i, k
real :: expected

allocate(c(1)%m_(2, 2))
allocate(c(2)%m_(3, 5))

o = -1.0
do concurrent (i = 1:4)
    block
        real, allocatable :: w(:)
        integer :: k
        allocate(w(size(c(2)%m_)))
        do k = 1, size(c(2)%m_)
            w(k) = real(1000 * i + k)
        end do
        o(i) = 0.0
        do k = 1, size(c(2)%m_)
            o(i) = o(i) + w(k)
        end do
        deallocate(w)
    end block
end do

do i = 1, 4
    expected = 0.0
    do k = 1, 15
        expected = expected + real(1000 * i + k)
    end do
    if (abs(o(i) - expected) > 1.0e-4) error stop "element-2 workspace"
end do

print *, "PASS"
end program
