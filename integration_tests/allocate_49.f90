program allocate_49
! Test allocate with mold= where the mold is an assumed-rank
! array inside a select rank block.
implicit none
real, allocatable :: a(:,:)
integer, allocatable :: b(:)
real, allocatable :: c(:,:)
real :: m(2,3)
integer :: n(5)
m = 1.0
n = 42

! Test 1: mold is assumed-rank, target is fixed-rank
call alloc_with_mold_real(a, m)
if (size(a, 1) /= 2) error stop
if (size(a, 2) /= 3) error stop

! Test 2: mold is assumed-rank, target is fixed-rank (integer)
call alloc_with_mold_int(b, n)
if (size(b) /= 5) error stop

! Test 3: both target and mold are assumed-rank
call alloc_both_assumed(c, m)
if (size(c, 1) /= 2) error stop
if (size(c, 2) /= 3) error stop

print *, "PASS"
contains
  subroutine alloc_with_mold_real(x, mold_arg)
    real, allocatable, intent(inout) :: x(:,:)
    real, intent(in) :: mold_arg(..)
    select rank(mold_arg)
      rank(2)
        allocate(x, mold=mold_arg)
    end select
  end subroutine

  subroutine alloc_with_mold_int(x, mold_arg)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: mold_arg(..)
    select rank(mold_arg)
      rank(1)
        allocate(x, mold=mold_arg)
    end select
  end subroutine

  subroutine alloc_both_assumed(x, mold_arg)
    real, allocatable, intent(inout) :: x(..)
    real, intent(in) :: mold_arg(..)
    select rank(x)
      rank(2)
        select rank(mold_arg)
          rank(2)
            allocate(x, mold=mold_arg)
        end select
    end select
  end subroutine
end program
