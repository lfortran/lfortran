module gpu_metal_223_mod
implicit none

type :: base_t
    integer :: m_
end type

type, extends(base_t) :: child_t
    integer :: k_
end type

contains

    pure function unit_vector(dir, length) result(u)
        integer, intent(in) :: dir, length
        real :: u(length)
        integer :: i
        do i = 1, length
            u(i) = 0.0
        end do
        u(dir) = real(length)
    end function

    ! Extent of the kernel workspace comes from a component of a
    ! POLYMORPHIC (class) dummy, used bare.
    subroutine bare_extent(self, d)
        class(base_t), intent(in) :: self
        real, intent(out) :: d(6,3)
        integer :: col
        d = 0.0
        do concurrent (col=1:3)
            d(1:3,col) = unit_vector(col, self%m_)
        end do
    end subroutine

    ! Same, but the extent is an arithmetic expression over the component.
    subroutine arith_extent(self, d)
        class(base_t), intent(in) :: self
        real, intent(out) :: d(6,3)
        integer :: col
        d = 0.0
        do concurrent (col=1:3)
            d(1:4,col) = unit_vector(col, self%m_ + 1)
        end do
    end subroutine

    ! Polymorphic dummy of an EXTENDING type, sized by its OWN component
    ! (the parent occupies slot 0 of the LLVM struct).
    subroutine own_extent(self, d)
        class(child_t), intent(in) :: self
        real, intent(out) :: d(6,3)
        integer :: col
        d = 0.0
        do concurrent (col=1:3)
            d(1:5,col) = unit_vector(col, self%k_)
        end do
    end subroutine

end module

program gpu_metal_223
use gpu_metal_223_mod
implicit none
type(base_t) :: b
type(child_t) :: c
real :: d(6,3)
integer :: i, j
real :: expected

b%m_ = 3
c%m_ = 3
c%k_ = 5

call bare_extent(b, d)
do j = 1, 3
    do i = 1, 3
        expected = 0.0
        if (i == j) expected = 3.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop "bare extent"
    end do
end do

call arith_extent(b, d)
do j = 1, 3
    do i = 1, 4
        expected = 0.0
        if (i == j) expected = 4.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop "arith extent"
    end do
end do

! A class(base_t) dummy that is actually handed an extended object.
call bare_extent(c, d)
do j = 1, 3
    do i = 1, 3
        expected = 0.0
        if (i == j) expected = 3.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop "bare extent, extended"
    end do
end do

call own_extent(c, d)
do j = 1, 3
    do i = 1, 5
        expected = 0.0
        if (i == j) expected = 5.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop "own extent"
    end do
end do

print *, "ok"
end program
