program coarray_teams_02
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team

    form team (1, team)
    sync team (team)

end program coarray_teams_02