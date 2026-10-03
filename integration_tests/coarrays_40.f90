program coarrays_40
  ! Unused team_type variables must not break compilation (issue #12538)
  use iso_fortran_env, only: team_type
  implicit none
  type(team_type) :: subteam
  type(team_type) :: default_team
  form team(1, subteam)
end program coarrays_40
