module gpu_metal_384_m
  implicit none
contains
  elemental real function evaluate(selection, x) result(y)
    integer, intent(in) :: selection
    real, intent(in) :: x
    select case (selection)
    case (1)
      y = 2*x
    case default
      error stop "evaluate: unknown activation"
    end select
  end function
end module

program gpu_metal_384
  ! Test: an `error stop` that is taken in a procedure an offloaded loop
  ! calls. The thread records it in the launch's status buffer and the host
  ! runs the same statement after the launch, so the program prints
  ! "ERROR STOP evaluate: unknown activation" and ends with status 1, as it
  ! does on the CPU. The test is registered FAIL: it checks that the program
  ! stops rather than reaching the print below.
  use gpu_metal_384_m
  implicit none
  integer, parameter :: n = 8
  real :: a(n)
  integer :: i, selection(n)

  selection = 1
  selection(5) = 3
  do concurrent (i = 1:n)
    a(i) = evaluate(selection(i), real(i))
  end do

  print *, "NOT REACHED", sum(a)
end program
