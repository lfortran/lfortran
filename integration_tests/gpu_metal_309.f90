! A BLOCK workspace sized from an inherited integer component. The
! host used to look the member up only on the child's Struct, miss it,
! and after accepting the launch size the buffer to zero. The parent
! component must be readable and the loop must compute the right sum.
program gpu_metal_309
implicit none
type :: base_t
    integer :: n
end type
type, extends(base_t) :: child_t
    real :: x
end type
type(child_t) :: s
real :: a(4)
integer :: i

s%n = 3
s%x = 1.0
a = 0.0
call run(a, s)
do i = 1, 4
    if (abs(a(i) - real(i * s%n * (s%n + 1) / 2)) > 1.0e-5) error stop
end do
print *, "PASS"

contains

    subroutine run(r, self)
        real, intent(out) :: r(:)
        type(child_t), intent(in) :: self
        integer :: i, k
        do concurrent (i = 1:size(r))
            block
                real, allocatable :: work(:)
                allocate(work(self%n))
                do k = 1, self%n
                    work(k) = real(k * i)
                end do
                r(i) = sum(work)
                deallocate(work)
            end block
        end do
    end subroutine

end program
