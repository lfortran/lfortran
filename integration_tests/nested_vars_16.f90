! A program type (a_t) captured by a contained procedure and then used in
! select type guards (type is / class is) and in an allocate type-spec, both
! in the program and in the contained procedure. nested_vars moves a_t into
! its context module, so these guards and type-specs must be re-pointed.
program nested_vars_16
  implicit none
  type :: a_t
    integer :: x = 1
  end type
  type, extends(a_t) :: b_t
    integer :: y = 2
  end type
  type(a_t) :: t
  class(a_t), pointer :: p
  integer :: n
  t = a_t(4)
  call s()
  if (t%x /= 5) error stop 1
  select type (p)
  type is (a_t)
    n = p%x
  class is (a_t)
    n = -1
  end select
  if (n /= 5) error stop 2
  deallocate(p)
  allocate(b_t :: p)
  select type (p)
  type is (a_t)
    n = -2
  class is (a_t)
    n = p%x + 10
  end select
  if (n /= 11) error stop 3
  deallocate(p)
  print *, n, t%x
contains
  subroutine s()
    allocate(a_t :: p)
    p%x = t%x + 1
    select type (p)
    type is (a_t)
      t = a_t(p%x)
    class is (a_t)
      t = a_t(-1)
    end select
  end subroutine
end program
