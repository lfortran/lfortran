module cond_03_mod
   implicit none

   type :: Integer32Complex32Pair
      integer :: first
      complex :: second
      contains
   end type Integer32Complex32Pair

   interface operator(==)
      module procedure map_p_equal
   end interface operator(==)

   contains

   logical function map_p_equal(a, b) result(equal)
      type(Integer32Complex32Pair), target, intent(in) :: a
      type(Integer32Complex32Pair), target, intent(in) :: b
      equal = (a%first == b%first) .and. (a%second == b%second)
   end function map_p_equal
end module cond_03_mod

program cond_03
  use cond_03_mod
  implicit none
  type(Integer32Complex32Pair), target :: p1, p2, p3
  type(Integer32Complex32Pair), pointer :: ptr_p1
  real, pointer :: d1, d2
  real, target :: t1, t2
  logical :: result

  ! Initialize target objects
  t1 = 4
  t2 = 5
  ! Assign pointers
  d1 => t1
  d2 => t2

  if ( d1 > d2 ) error stop "Test_1 failed"

  t1 = 7
  t2 = 6

  if ( d1 < d2 ) error stop "Test_2 failed"

  ptr_p1 => p1
  p1%first = 10
  p1%second = (3.0, 4.0)

  p2%first = 10
  p2%second = (3.0, 4.0)

  p3%first = 20
  p3%second = (3.0, 4.0)

  result = (ptr_p1 == p2)
  print *, "result: ", result
  if (.not. result) error stop

  result = (p2 == ptr_p1)
  print *, "result: ", result
  if (.not. result) error stop

  result = (ptr_p1 == p3)
  print *, "result: ", result
  if (result) error stop

  result = (p3 == ptr_p1)
  print *, "result: ", result
  if (result) error stop
end program cond_03
