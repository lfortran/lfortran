program coarrays_40
  ! Unused team_type variables must not break compilation (issue #12538)
  use iso_fortran_env, only: team_type
  implicit none

  type(team_type) :: subteam
  type(team_type) :: default_team
  integer :: n

  n = num_images()
  form team(1, subteam)
  if (num_images() /= n) error stop 1
end program coarrays_40
