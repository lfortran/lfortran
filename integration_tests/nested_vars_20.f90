! An allocatable derived-type scalar of a host procedure used in its internal
! procedures, which read, modify, deallocate and allocate it (#13602).
module nested_vars_20_mod
  implicit none
  type :: a_t
    integer :: x, y
  end type
  type, extends(a_t) :: b_t
    integer :: z
  end type
contains
  subroutine s2()
    type(a_t), allocatable :: t
    allocate(t)
    t%x = 5
    print *, f()
    if (f() /= 5) error stop 1
    call bump()
    if (t%x /= 6) error stop 2
    call drop()
    if (allocated(t)) error stop 3
    call set()
    if (.not. allocated(t)) error stop 4
    if (t%x /= 1 .or. t%y /= 2) error stop 5
  contains
    integer function f()
      f = t%x
    end function
    subroutine bump()
      t%x = t%x + 1
    end subroutine
    subroutine drop()
      if (.not. allocated(t)) error stop 6
      deallocate(t)
    end subroutine
    subroutine set()
      if (allocated(t)) error stop 7
      t = a_t(1, 2)
    end subroutine
  end subroutine

  subroutine s3()
    class(a_t), allocatable :: c
    allocate(b_t :: c)
    c%x = 3
    call set_z()
    select type (c)
    type is (b_t)
      if (c%z /= 4) error stop 8
    class default
      error stop 9
    end select
  contains
    subroutine set_z()
      select type (c)
      type is (b_t)
        c%z = c%x + 1
      end select
    end subroutine
  end subroutine
end module

program nested_vars_20
  use nested_vars_20_mod
  implicit none
  call s2()
  call s3()
end program
