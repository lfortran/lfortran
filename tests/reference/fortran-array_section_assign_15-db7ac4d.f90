module array_section_assign_15_m
implicit none
type :: tensor_t
    real(4), dimension(:), allocatable :: values_
end type tensor_t
type :: pair_t
    type(tensor_t) :: inputs_
contains
    procedure :: inputs
end type pair_t
type :: particle
    real(4), dimension(:), allocatable :: d
    real(4), dimension(:), allocatable :: r
end type particle

contains

subroutine elemental_results(pairs, in)
    integer(4) :: __libasr_created__result_metadata_
    integer(4) :: __libasr_created__result_metadata_1
    type(tensor_t), dimension(:), allocatable, intent(inout) :: in
    type(pair_t), dimension(:), intent(in) :: pairs
    __libasr_created__result_metadata_ = size(pairs, 1)
    __libasr_created__result_metadata_1 = size(pairs, 1)
    in = pairs%inputs()
end subroutine elemental_results

elemental type(tensor_t) function inputs(self) result(t)
    class(pair_t), intent(in) :: self
    t = self%inputs_
end function inputs

subroutine pair_differences(p, i, j)
    integer(4), intent(in) :: i
    integer(4), intent(in) :: j
    type(particle), dimension(:), intent(inout) :: p
    p(i)%d = p(j)%r - p(i)%r
end subroutine pair_differences

subroutine self_update(h)
    real(4), dimension(:), allocatable, intent(inout) :: h
    h = 5.00000000e-01*h + 2.50000000e-01
end subroutine self_update

pure function twice(a) result(r)
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(size(a)) :: r
    r = real(2, kind=4)*a
end function twice

subroutine values(a, m, a2, b2, c1, c2, c3, c4)
    real(4), dimension(:), pointer :: __libasr_created__function_call_twice
    real(4), dimension(:), pointer :: __libasr_created__intrinsic_array_function_Cshift
    integer(4) :: __libasr_created__result_metadata_
    real(4), dimension(:), allocatable, intent(in) :: a
    real(4), dimension(:, :), allocatable, intent(in) :: a2
    real(4), dimension(:, :), allocatable, intent(in) :: b2
    real(4), dimension(:), allocatable, intent(inout) :: c1
    real(4), dimension(:), allocatable, intent(inout) :: c2
    real(4), dimension(:), allocatable, intent(inout) :: c3
    real(4), dimension(:, :), allocatable, intent(inout) :: c4
    logical(4), dimension(:), intent(in) :: m
    __libasr_created__result_metadata_ = 1*(0 > (3 - 1 + 1)/1 ? 0 : (3 - 1 + 1)/1)
    __libasr_created__function_call_twice => a(1:3)
    c1 = twice(__libasr_created__function_call_twice)
    __libasr_created__intrinsic_array_function_Cshift => a(1:3)
    ! CheckBounds LHS = c2, RHS = c2, 
    c2 = Cshift(__libasr_created__intrinsic_array_function_Cshift, 1)
    ! CheckBounds LHS = c3, RHS = c3, 
    c3 = Pack(a, m)
    ! CheckBounds LHS = c4, RHS = c4, 
    c4 = MatMul(a2, b2)
end subroutine values

end module array_section_assign_15_m

program array_section_assign_15
use array_section_assign_15_m, only: elemental_results
use array_section_assign_15_m, only: inputs
use array_section_assign_15_m, only: pair_differences
use array_section_assign_15_m, only: pair_t
use array_section_assign_15_m, only: particle
use array_section_assign_15_m, only: self_update
use array_section_assign_15_m, only: tensor_t
use array_section_assign_15_m, only: twice
use array_section_assign_15_m, only: values
implicit none
real(4), dimension(:), allocatable :: a
real(4), dimension(:, :), allocatable :: a2
real(4), dimension(:, :), allocatable :: b2
real(4), dimension(:), allocatable :: c1
real(4), dimension(3), parameter :: c1_ref = [2.00000000e+00, 4.00000000e+00, 6.00000000e+00]
real(4), dimension(:), allocatable :: c2
real(4), dimension(3), parameter :: c2_ref = [2.00000000e+00, 3.00000000e+00, 1.00000000e+00]
real(4), dimension(:), allocatable :: c3
real(4), dimension(3), parameter :: c3_ref = [1.00000000e+00, 3.00000000e+00, 5.00000000e+00]
real(4), dimension(:, :), allocatable :: c4
real(4), dimension(:), allocatable :: h
real(4), dimension(4), parameter :: h_ref = [7.50000000e-01, 1.25000000e+00, 1.75000000e+00, 2.25000000e+00]
integer(4) :: i
type(tensor_t), dimension(:), allocatable :: in
integer(4) :: j
logical(4), dimension(:), allocatable :: m
type(particle), dimension(2) :: p
type(pair_t), dimension(3) :: pairs
allocate(h(4))
do i = 1, 4
    h(i) = real(i, kind=4)
end do
call self_update(h)
do i = 1, 4
    if (h(i) /= h_ref(i)) then
        error stop
    end if
end do
do i = 1, 2
    allocate(p(i)%r(3), p(i)%d(3))
    do j = 1, 3
        p(i)%r(j) = real(i*j, kind=4)
        p(i)%d(j) = real(0, kind=4)
    end do
end do
call pair_differences(p, 1, 2)
call pair_differences(p, 2, 2)
do j = 1, 3
    if (p(1)%d(j) /= real(j, kind=4)) then
        error stop
    end if
    if (p(2)%d(j) /= real(0, kind=4)) then
        error stop
    end if
end do
allocate(in(3))
do i = 1, 3
    allocate(pairs(i)%inputs_%values_(2), in(i)%values_(2))
    pairs(i)%inputs_%values_(1) = real(i, kind=4)
    pairs(i)%inputs_%values_(2) = real(2*i, kind=4)
    in(i)%values_(1) = real(0, kind=4)
    in(i)%values_(2) = real(0, kind=4)
end do
call elemental_results(pairs, in)
pairs(1)%inputs_%values_(1) = real(7, kind=4)
do i = 1, 3
    if (size(in(i)%values_) /= 2) then
        error stop
    end if
    if (in(i)%values_(1) /= real(i, kind=4) .or. in(i)%values_(2) /= real(2*i, kind=4)) then
        error stop
    end if
end do
allocate(a(5), m(5), c1(3), c2(3), c3(3), a2(3, 3), b2(3, 3), c4(3, 3))
do i = 1, 5
    a(i) = real(i, kind=4)
    m(i) = Mod(i, 2) == 1
end do
do j = 1, 3
    do i = 1, 3
        a2(i, j) = real(i, kind=4)
        b2(i, j) = real(j, kind=4)
    end do
end do
call values(a, m, a2, b2, c1, c2, c3, c4)
do i = 1, 3
    if (c1(i) /= c1_ref(i)) then
        error stop
    end if
    if (c2(i) /= c2_ref(i)) then
        error stop
    end if
    if (c3(i) /= c3_ref(i)) then
        error stop
    end if
    do j = 1, 3
        if (c4(i, j) /= real(3*i*j, kind=4)) then
            error stop
        end if
    end do
end do
print *, h, c3
end program array_section_assign_15
