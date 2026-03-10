! Test: identically-named derived types in different contained
! subroutines with class(*) arrays and select type.
! Regression test for LLVM type confusion in array descriptor cache
! when two scopes define types with the same name.
program select_type_35
  implicit none
  call sub1()
  call sub2()
  print *, "ok"
contains
  subroutine sub1()
    type point
      real :: x, y
    end type
    class(*), allocatable :: val(:)
    allocate(val, source=[point(1.0,2.0), point(3.0,4.0)])
    select type (val)
    type is (point)
      if (abs(val(1)%x - 1.0) > 1e-6) error stop "sub1 failed"
      if (abs(val(2)%y - 4.0) > 1e-6) error stop "sub1 failed"
    end select
  end subroutine
  subroutine sub2()
    type point
      real :: x, y
    end type
    class(*), allocatable :: val(:)
    allocate(val, source=[point(5.0,6.0)])
    select type (val)
    type is (point)
      if (abs(val(1)%x - 5.0) > 1e-6) error stop "sub2 failed"
    end select
  end subroutine
end program
