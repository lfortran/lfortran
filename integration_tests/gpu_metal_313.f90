! A polymorphic (`class`) dummy whose declared type has an allocatable array
! component, shared into an offloaded `do concurrent`.
!
! A `class` dummy is represented by a container holding a type descriptor
! beside a pointer to the data, while the kernel is generated against the
! declared type, so the launch copies the declared type's own components into
! a plain local and hands that over. That copy used to be abandoned as soon as
! the type had any allocatable component, and the container itself was
! uploaded instead: reading `self%m_` on the device then read the low half of
! the host data pointer, so every element came out a different garbage number
! from run to run.
module gpu_metal_313_mod
    implicit none

    type :: op_t
        real, allocatable :: w(:)
        integer :: m_
        integer :: k_
    end type

contains

    function assemble(self, n) result(a)
        class(op_t), intent(in) :: self
        integer, intent(in) :: n
        integer, allocatable :: a(:)
        allocate(a(n))
        do concurrent (integer :: i = 1:n) default(none) shared(a, self, n)
            a(i) = self%m_ + 1000 * self%k_ + i
        end do
    end function

end module gpu_metal_313_mod

program gpu_metal_313
use gpu_metal_313_mod, only: op_t, assemble
implicit none

type(op_t) :: op
integer, allocatable :: a(:)
integer :: i

allocate(op%w(3))
op%w = 1.0
op%m_ = 100
op%k_ = 7

a = assemble(op, 8)

do i = 1, 8
    if (a(i) /= 7100 + i) error stop
end do

print *, "PASS"

end program
