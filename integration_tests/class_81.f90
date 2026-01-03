module bar_type

  implicit none
  private

  public :: bar_destroy

  type, public :: bar
     integer :: v = 0
  contains
    final :: bar_destroy
  end type bar

  interface bar_destroy
    module procedure bar_destroy
  end interface bar_destroy

contains

  subroutine bar_destroy(this)
    type(bar), intent(inout) :: this
    this%v = -1
  end subroutine

end module bar_type


module foo_type

  implicit none

  type :: foo
  contains
    procedure :: f
  end type

contains

  subroutine f(this, x)
    use bar_type, only: bar
    class(foo), intent(in) :: this
    type(bar), intent(in) :: x

    print *, "foo%f called, x%v =", x%v
    if (x%v /= 42) error stop
  end subroutine

end module foo_type


program class_81

  use foo_type
  use bar_type

  implicit none
  
  type(foo) :: a
  type(bar) :: b

  b%v = 42

  print *, "Before call: b%v =", b%v
  if (b%v /= 42) error stop
  call a%f(b)
  print *, "After call: b%v =", b%v
  if (b%v /= 42) error stop

end program class_81