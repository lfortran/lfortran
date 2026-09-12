! Allocatable array components INHERITED from an extended type must be
! plumbed into a Metal kernel just like the type's own components.  The
! backend models inheritance as a nested `__parent` field, so inherited
! components are not listed in the child type's own member list; the
! member enumeration has to walk the parent chain, otherwise neither the
! `__size_...` scalar nor the `__data_...` buffer is passed and
! `size(self%inner_)` lowers to an invalid placeholder.
program gpu_metal_234
implicit none

type :: grandparent_t
    real, allocatable :: g_(:)
end type

type, extends(grandparent_t) :: base_t
    real, allocatable :: inner_(:)
    real :: fixed_(2)
end type

type, extends(base_t) :: child_t
    real, allocatable :: own_(:)
    integer :: m_
end type

type(child_t) :: s
real :: d(4), e(4)
integer :: j

allocate(s%g_(3))
allocate(s%inner_(2))
allocate(s%own_(5))
s%g_ = [10.0, 20.0, 30.0]
s%inner_ = [1.0, 2.0]
s%own_ = [7.0, 8.0, 9.0, 10.0, 11.0]
s%fixed_(1) = 100.0
s%fixed_(2) = 200.0
s%m_ = 3

! sizes of an inherited (base), a grandparent and an own component
d = 0.0
do concurrent (j = 1:4)
    d(j) = sizes(s)
end do
do j = 1, 4
    if (abs(d(j) - 13.0) > 1.0e-6) error stop "sizes"
end do

! element reads through the same inherited components
e = 0.0
do concurrent (j = 1:4)
    e(j) = elems(s)
end do
do j = 1, 4
    if (abs(e(j) - 348.0) > 1.0e-6) error stop "elems"
end do

print *, d(1), e(1)
print *, "ok"

contains

    pure function sizes(self) result(r)
        class(child_t), intent(in) :: self
        real :: r
        ! 2 + 3 + 5 + 3 = 13
        r = real(size(self%inner_) + size(self%g_) + size(self%own_) &
            + self%m_)
    end function

    pure function elems(self) result(r)
        class(child_t), intent(in) :: self
        real :: r
        ! 1 + 2 + 10 + 30 + 7 + 11 + 100 + 200 - 3 = 358 - 10 = 348
        r = self%inner_(1) + self%inner_(2) &
            + self%g_(1) + self%g_(3) &
            + self%own_(1) + self%own_(5) &
            + self%fixed_(1) + self%fixed_(2) &
            - real(self%m_) - 10.0
    end function

end program
