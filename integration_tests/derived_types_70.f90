program derived_types_70

  implicit none
  type :: task
    logical :: done
  end type task
  type :: container
    type(task), allocatable :: deps(:)
  end type container
  class(container), allocatable :: x
  logical :: finished
  integer :: i
  allocate(x)
  allocate(x%deps(3))
  call temp(x)
  if (any(x%deps(:)%done .neqv. [.true., .false., .true.])) error stop

contains 

  subroutine temp(tab)
    class(container), intent(inout) :: tab
    tab%deps(1)%done = .true.
    tab%deps(2)%done = .false.
    tab%deps(3)%done = .true.
  end subroutine

end program
