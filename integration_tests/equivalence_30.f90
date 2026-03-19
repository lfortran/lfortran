module mre_verify
  implicit none
  private

  integer, save, public :: unit_test_level = 5

  public :: unit_check_level
  integer :: unit_check_level
  equivalence (unit_test_level, unit_check_level)
end module mre_verify

program equivalence_30
  use mre_verify, only : unit_test_level
  implicit none
  print *, unit_test_level
  if (unit_test_level /= 5) error stop
end program equivalence_30
