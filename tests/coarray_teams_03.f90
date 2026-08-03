! run with <= 4 images
program coarray_teams_03
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team
    integer :: team_num, old_image
    integer :: expected_image(4)

    expected_image = [1, 2, 1, 2]

    old_image = this_image()
    team_num = (old_image - 1) / 2 + 1

    form team (team_num, team)

    change team (team)
        ! TODO: Uncomment once TEAM_NUMBER() is implemented
        !if (team_number() /= team_num) then
        !    error stop "incorrect team number"
        !end if

        ! NOTE: this property is not technically guaranteed without FORM TEAM(NEW_INDEX=),
        ! but it's true of all known implementations
        if (this_image() /= expected_image(old_image)) then
            error stop "incorrect image index"
        end if
    end team
end program coarray_teams_03