program coarray_teams_01
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team1, team2
    integer :: team_num
    integer :: new_index
    integer :: iostat
    character(len=64) :: errmsg

    team_num = mod(this_image() - 1, 2) + 1

    ! Basic FORM TEAM
    form team (1, team1)

    ! Variable team number
    form team (team_num, team2)

    ! Optional specifiers
    form team (1, team1, new_index=1)
    form team (1, team1, stat=iostat)
    form team (1, team1, errmsg=errmsg)
    form team (team_num, team2, new_index=(this_image()-1)/2+1, stat=iostat, errmsg=errmsg)

end program coarray_teams_01