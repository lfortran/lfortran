module associate_46_mod
  implicit none
  abstract interface
    function func_i() result(r)
      logical :: r
    end function
  end interface
  type :: t
    integer :: val = 0
  end type
  interface t
    module procedure create
  end interface
contains
  function create(desc, f) result(r)
    character(len=*), intent(in) :: desc
    procedure(func_i), intent(in), pointer, optional :: f
    type(t) :: r
    r%val = len(desc)
  end function
end module

program associate_46
  use associate_46_mod, only : t
  implicit none
  type(t) :: x
  associate(me => 1)
    x = t("hello")
  end associate
  if (x%val /= 5) error stop
  print *, x%val
end program
