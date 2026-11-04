! Run with --descriptor-index-64: the allocatable components of an array of a
! derived type reach the kernel as the components' data laid end to end, with
! 64-bit offsets and extents.
! a) Components of different lengths, one of them allocated with no elements,
!    so every element starts somewhere else in the packed data; the kernel
!    updates the last element of each component and the host checks that
!    every element landed back in its own component (as gpu_metal_716).
! b) The whole component of each element is assigned a type-bound function's
!    result inside the kernel. Without reallocation on assignment, the kernel
!    copies the per-thread result into the packed data from the element's
!    offset there, which it reads into a local first: that local is 64-bit
!    like the offsets, so a component starting at or beyond huge(0) elements
!    into the packed data is written in place.
module gpu_metal_717_m
    implicit none
    type :: t
        integer, allocatable :: v(:)
    contains
        procedure :: shifted
    end type
contains
    pure function shifted(self, k) result(w)
        class(t), intent(in) :: self
        integer, intent(in) :: k
        integer, allocatable :: w(:)
        w = self%v + k
    end function
end module

program gpu_metal_717
    use gpu_metal_717_m, only: t
    implicit none
    type :: s
        integer, allocatable :: v(:)
    end type
    type(s) :: a(4)
    type(t) :: b(4)
    integer :: i, j, n(4)
    n = [5, 0, 12, 7]

    do i = 1, 4
        if (i == 2) cycle
        allocate(a(i)%v(n(i)))
        do j = 1, n(i)
            a(i)%v(j) = 100*i + j
        end do
    end do
    allocate(a(2)%v(0))
    do concurrent (i = 1:4, n(i) > 0)
        a(i)%v(n(i)) = a(i)%v(n(i)) + 1000*i
    end do
    print *, a(1)%v(n(1)), a(3)%v(n(3)), a(4)%v(n(4))
    if (size(a(2)%v) /= 0) error stop 1
    do i = 1, 4
        do j = 1, n(i) - 1
            if (a(i)%v(j) /= 100*i + j) error stop 2
        end do
        if (n(i) > 0) then
            if (a(i)%v(n(i)) /= 1100*i + n(i)) error stop 3
        end if
    end do

    do i = 1, 4
        allocate(b(i)%v(n(i)))
        do j = 1, n(i)
            b(i)%v(j) = 100*i + j
        end do
    end do
    do concurrent (i = 1:4)
        b(i)%v = b(i)%shifted(10*i)
    end do
    print *, b(4)%v
    do i = 1, 4
        if (size(b(i)%v) /= n(i)) error stop 4
        do j = 1, n(i)
            if (b(i)%v(j) /= 110*i + j) error stop 5
        end do
    end do
end program
