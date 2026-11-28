program p
  implicit none
  type, private :: t1
    integer :: c = 1
  end type
  type, public :: t2
    integer :: c = 2
  end type
end program
