program forall_04
implicit none
real :: a(3,3)
integer :: i

a = 1.0
call zero_upper(a)

! Expected: lower-triangular matrix
! Row 1: 1.0 0.0 0.0
! Row 2: 1.0 1.0 0.0
! Row 3: 1.0 1.0 1.0
if (a(1,1) /= 1.0) error stop
if (a(1,2) /= 0.0) error stop
if (a(1,3) /= 0.0) error stop
if (a(2,1) /= 1.0) error stop
if (a(2,2) /= 1.0) error stop
if (a(2,3) /= 0.0) error stop
if (a(3,1) /= 1.0) error stop
if (a(3,2) /= 1.0) error stop
if (a(3,3) /= 1.0) error stop

! Test with a larger array
call test_5x5()

print *, "All tests passed."

contains

subroutine zero_upper(a)
    real, intent(inout) :: a(:,:)
    integer :: j
    forall (j=2:size(a,2)) a(:j-1,j) = 0.0
end subroutine

subroutine test_5x5()
    real :: b(5,5)
    integer :: i, j
    b = 1.0
    call zero_upper(b)
    do j = 1, 5
        do i = 1, 5
            if (i < j) then
                if (b(i,j) /= 0.0) error stop
            else
                if (b(i,j) /= 1.0) error stop
            end if
        end do
    end do
end subroutine

end program
