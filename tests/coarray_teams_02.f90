program sync_team_mre
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team

    form team (1, team)
    sync team (team)

end program sync_team_mre