! `size(m, dim)` on an allocatable array component of a struct passed into
! a Metal kernel used to return the total number of elements, because only
! one flat `__size_<var>_<member>` scalar was ever passed down and the
! `dim` argument was dropped.  The per-dimension extents are now plumbed
! through as well.  The component is 2x3 so that 2, 3 and 6 are all
! distinguishable.
program gpu_metal_221
implicit none

type :: base_t
    real, allocatable :: up_(:,:)
end type

type, extends(base_t) :: child_t
    real, allocatable :: own_(:,:)
end type

type(child_t) :: s
integer :: d(4), e(4)
integer :: j

allocate(s%up_(2,3))
allocate(s%own_(4,5))
s%up_ = 1.0
s%own_ = 2.0

! Extents reached through a device function: own and inherited component.
d = 0
do concurrent (j = 1:4)
    d(j) = extents(s)
end do
do j = 1, 4
    if (d(j) /= 23456) error stop "extents"
end do

! Total size must still be the product of the extents.
e = 0
do concurrent (j = 1:4)
    e(j) = totals(s)
end do
do j = 1, 4
    if (e(j) /= 2006) error stop "totals"
end do

print *, d(1), e(1)
print *, "ok"

contains

    pure function extents(self) result(r)
        class(child_t), intent(in) :: self
        integer :: r
        ! 2, 3, 4, 5 -> 23456, none of which is the total (6 or 20)
        r = 10000 * size(self%up_, 1) + 1000 * size(self%up_, 2) &
            + 100 * size(self%own_, 1) + 10 * size(self%own_, 2) &
            + 6
    end function

    pure function totals(self) result(r)
        class(child_t), intent(in) :: self
        integer :: r
        ! 6 + 2000 = 2006
        r = size(self%up_) + 100 * size(self%own_)
    end function

end program
