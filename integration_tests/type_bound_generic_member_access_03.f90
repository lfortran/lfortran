module type_bound_generic_member_access_03_m
  implicit none
  logical :: called = .false.
  type :: runner
  contains
    procedure :: run_a1
    procedure :: run_a2
    generic :: run => run_a1, run_a2
  end type
contains
  subroutine run_a1(this, a1, f)
    class(runner), intent(inout) :: this
    class(*), intent(in) :: a1
    procedure() :: f
    call f()
  end subroutine

  subroutine run_a2(this, a1, f2, arg)
    class(runner), intent(inout) :: this
    class(*), intent(in) :: a1
    procedure() :: f2
    integer, intent(in) :: arg
    call f2(arg)
  end subroutine
end module

program type_bound_generic_member_access_03
  use type_bound_generic_member_access_03_m
  implicit none
  type(runner) :: br
  logical :: called_with_arg = .false.

  call br%run(1, my_sub)
  if (.not. called) error stop 1

  call br%run(1, my_sub_with_arg, 42)
  if (.not. called_with_arg) error stop 2

contains
  subroutine my_sub_with_arg(x)
    integer, intent(in) :: x
    if (x == 42) called_with_arg = .true.
  end subroutine
  subroutine my_sub()
    use type_bound_generic_member_access_03_m, only: called
    called = .true.
  end subroutine
end program
