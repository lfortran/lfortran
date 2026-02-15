! Test: associate with implied-do array constructor (runtime-sized)
module associate_30_mod
  implicit none
contains
  logical function f(args)
    character(len=*), intent(in) :: args(:)
    integer :: i
    associate(lengths => [(len(trim(args(i))), i = 1, size(args))])
      f = any(lengths > 0)
    end associate
  end function
end module

program associate_30
  use associate_30_mod, only : f
  logical :: res
  res = f(["hello"])
  if (.not. res) error stop
  res = f(["hello", "world"])
  if (.not. res) error stop
  res = f(["     "])
  if (res) error stop
end program
