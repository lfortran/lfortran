module pass_array_by_data_13_mod
  implicit none
contains
  subroutine outer(a)
    integer, intent(inout) :: a(:)
    call inner(a, 1)
  contains
    recursive subroutine inner(a, i)
      integer, intent(inout) :: a(:)
      integer, intent(in) :: i
      if (i < 2) then
        call inner(a, i + 1)
      end if
      a = a
    end subroutine inner
  end subroutine outer
end module pass_array_by_data_13_mod

program pass_array_by_data_13
  use pass_array_by_data_13_mod, only: outer
  implicit none
  integer :: a(2)
  a = [1, 2]
  call outer(a)
  if (any(a /= [1, 2])) error stop
end program pass_array_by_data_13
