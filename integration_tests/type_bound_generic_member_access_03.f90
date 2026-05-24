module type_bound_generic_member_access_03_m
  implicit none
  logical :: called = .false.
  type :: runner
  contains
    procedure :: run_a1
    generic :: run => run_a1
  end type
contains
  subroutine run_a1(this, a1, f)
    class(runner), intent(inout) :: this
    class(*), intent(in) :: a1
    procedure() :: f
    call f()
  end subroutine
end module

program type_bound_generic_member_access_03
  use type_bound_generic_member_access_03_m
  implicit none
  type(runner) :: br
  interface
    subroutine my_sub()
    end subroutine
  end interface
  call br%run(1, my_sub)
  if (.not. called) error stop 1
end program

subroutine my_sub()
  use type_bound_generic_member_access_03_m, only: called
  called = .true.
end subroutine
