! Procedures that realloc_lhs.f90 calls from its hot loops. This file is
! compiled separately and linked, so that the compiler of the driver cannot
! inline them or see what they do: the shape check of an assignment in them
! runs on every call, and a call of one of them is opaque to the loop that
! makes it. They are external procedures, not module procedures, which some
! compilers (LFortran) compile into each object that uses their module;
! realloc_lhs.f90 declares their interfaces. See README.md in this
! directory.

! An opaque call: the caller cannot know that it touches nothing but `k`.
subroutine ext_tick(k)
implicit none
integer, intent(inout) :: k
k = k + 1
end subroutine

! c = a + b with distinct allocatable dummies
subroutine ext_add(a, b, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), allocatable, intent(inout) :: a(:), b(:), c(:)
c = a + b
end subroutine

! c = a + b, rank 2
subroutine ext_add2d(a, b, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), allocatable, intent(inout) :: a(:,:), b(:,:), c(:,:)
c = a + b
end subroutine

! c = a + b, rank 3
subroutine ext_add3d(a, b, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), allocatable, intent(inout) :: a(:,:,:), b(:,:,:), c(:,:,:)
c = a + b
end subroutine

! Three chained assignments, the first two to allocatable dummies that
! serve as work arrays: t1 = a + b; t2 = t1*a; c = t2 - b
subroutine ext_chain(a, b, t1, t2, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), intent(in) :: a(:), b(:)
real(dp), allocatable, intent(inout) :: t1(:), t2(:), c(:)
t1 = a + b
t2 = t1*a
c = t2 - b
end subroutine

! The same with local work arrays, allocated explicitly with the shape of
! `a` (valid without reallocation on assignment)
subroutine ext_chain_alloc(a, b, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), intent(in) :: a(:), b(:)
real(dp), allocatable, intent(inout) :: c(:)
real(dp), allocatable :: t1(:), t2(:)
allocate(t1(size(a)), t2(size(a)))
t1 = a + b
t2 = t1*a
c = t2 - b
end subroutine

! The same with local work arrays that the assignments allocate (requires
! reallocation on assignment)
subroutine ext_chain_local(a, b, c)
implicit none
integer, parameter :: dp = kind(1.d0)
real(dp), intent(in) :: a(:), b(:)
real(dp), allocatable, intent(inout) :: c(:)
real(dp), allocatable :: t1(:), t2(:)
t1 = a + b
t2 = t1*a
c = t2 - b
end subroutine
