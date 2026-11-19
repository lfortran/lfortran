module array_section_assign_18_m
implicit none
type :: particle
    real(4), dimension(:), allocatable :: d
    real(4), dimension(:), allocatable :: r
end type particle

contains

subroutine kernels(x, y, p, c2, a2, b2, e)
    real(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Cshift
    integer(4) :: __libasr_created__result_metadata_
    real(4), dimension(:, :), intent(in) :: a2
    real(4), dimension(:, :), intent(in) :: b2
    real(4), dimension(:, :), allocatable, intent(inout) :: c2
    real(4), dimension(:), intent(inout) :: e
    type(particle), dimension(:), intent(inout) :: p
    real(4), dimension(:), allocatable, intent(inout) :: x
    real(4), dimension(:), intent(in) :: y
    x = 5.00000000e-01*x + y
    p(1)%d = p(2)%r - p(1)%r
    ! CheckBounds LHS = c2, RHS = c2, 
    c2 = MatMul(a2, b2)
    __libasr_created__result_metadata_ = size(y)
    e = twice(y)
    deallocate(__libasr_created__intrinsic_array_function_Cshift)
    allocate(__libasr_created__intrinsic_array_function_Cshift(size(x, 1)))
    __libasr_created__intrinsic_array_function_Cshift = Cshift(x, 1)
    x = __libasr_created__intrinsic_array_function_Cshift
end subroutine kernels

pure function twice(a) result(r)
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(size(a)) :: r
    r = real(2, kind=4)*a
end function twice

end module array_section_assign_18_m

program array_section_assign_18
use array_section_assign_18_m, only: kernels
use array_section_assign_18_m, only: particle
use array_section_assign_18_m, only: twice
implicit none
real(4), dimension(3), parameter :: __libasr_created__array_constant_ = [2.00000000e+00, 4.00000000e+00, 6.00000000e+00]
logical(4), dimension(:), allocatable :: __libasr_created__intrinsic_array_function_Any
logical(4) :: __libasr_created__intrinsic_array_function_Any1
logical(4) :: __libasr_created__intrinsic_array_function_Any2
logical(4), dimension(3) :: __libasr_created__intrinsic_array_function_Any3
logical(4) :: __libasr_created__intrinsic_array_function_Any4
logical(4) :: __libasr_created__intrinsic_array_function_Any5
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_ = [4.00000000e+00, 6.00000000e+00,&
         2.00000000e+00]
real(4), dimension(3), parameter :: __libasr_created_real_compare_right_1 = [2.00000000e+00, 4.00000000e+00,&
         6.00000000e+00]
real(4), dimension(2, 2) :: a2
real(4), dimension(2, 2) :: b2
real(4), dimension(:, :), allocatable, target :: c2
real(4), dimension(3) :: e
integer(4) :: i
type(particle), dimension(2) :: p
real(4), dimension(:, :), pointer :: pc
real(4), dimension(:), pointer :: px
real(4), dimension(:), allocatable, target :: x
real(4), dimension(3) :: y
allocate(x(3), c2(2, 2))
x = __libasr_created__array_constant_
y = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
a2 = real(1, kind=4)
b2 = real(2, kind=4)
e = real(0, kind=4)
px => x
pc => c2
do i = 1, 2
    allocate(p(i)%r(3), p(i)%d(3))
    p(i)%r = real(i, kind=4)
    p(i)%d = real(0, kind=4)
end do
call kernels(x, y, p, c2, a2, b2, e)
if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 2) then
    error stop
end if
if (.not. associated(px, x)) then
    error stop
end if
if (.not. associated(pc, c2)) then
    error stop
end if
deallocate(__libasr_created__intrinsic_array_function_Any)
allocate(__libasr_created__intrinsic_array_function_Any(size(x, 1)))
__libasr_created__intrinsic_array_function_Any = x /= __libasr_created_real_compare_right_
__libasr_created__intrinsic_array_function_Any4 = Any(__libasr_created__intrinsic_array_function_Any)
if (__libasr_created__intrinsic_array_function_Any4) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any1 = Any(p(1)%d /= real(1, kind=4))
if (__libasr_created__intrinsic_array_function_Any1) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any2 = Any(c2 /= real(4, kind=4))
if (__libasr_created__intrinsic_array_function_Any2) then
    error stop
end if
__libasr_created__intrinsic_array_function_Any3 = e /= __libasr_created_real_compare_right_1
__libasr_created__intrinsic_array_function_Any5 = Any(__libasr_created__intrinsic_array_function_Any3)
if (__libasr_created__intrinsic_array_function_Any5) then
    error stop
end if
x = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
if (.not. associated(px, x) .or. size(x) /= 3) then
    error stop
end if
print *, x, e
end program array_section_assign_18
