program coarrays_34
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team1
    type(team_type) :: team2
    integer :: original_image
    integer :: team_num
    integer :: nested_team_num
    integer :: stat

    original_image = this_image()

    ! Requires exactly 4 images.
    if (num_images() /= 4) error stop 1

    ! Team 1: original images 1, 2
    ! Team 2: original images 3, 4
    if (original_image <= 2) then
        team_num = 1
    else
        team_num = 2
    end if

    form team(team_num, team1)

    ! FORM TEAM itself must not change the current team.
    if (this_image() /= original_image) error stop 2
    if (num_images() /= 4) error stop 3

    stat = -1

    change team(team1, stat=stat)

        if (stat /= 0) error stop 4

        ! Each first-level team has two images.
        if (num_images() /= 2) error stop 5

        ! Verify team-relative image numbering.
        if (original_image == 1 .and. this_image() /= 1) error stop 6
        if (original_image == 2 .and. this_image() /= 2) error stop 7
        if (original_image == 3 .and. this_image() /= 1) error stop 8
        if (original_image == 4 .and. this_image() /= 2) error stop 9

        ! SYNC ALL now operates within the current team.
        sync all

        ! Split each two-image team into one-image teams.
        nested_team_num = this_image()

        form team(nested_team_num, team2)

        ! FORM TEAM must not itself change the current team.
        if (num_images() /= 2) error stop 10

        change team(team2, stat=stat)

            if (stat /= 0) error stop 11

            ! Every nested team contains exactly one image.
            if (num_images() /= 1) error stop 12
            if (this_image() /= 1) error stop 13

            sync all

        end team(stat=stat)

        if (stat /= 0) error stop 14

        ! END TEAM must restore the first-level team.
        if (num_images() /= 2) error stop 15

        if (original_image == 1 .and. this_image() /= 1) error stop 16
        if (original_image == 2 .and. this_image() /= 2) error stop 17
        if (original_image == 3 .and. this_image() /= 1) error stop 18
        if (original_image == 4 .and. this_image() /= 2) error stop 19

        sync all

    end team(stat=stat)

    if (stat /= 0) error stop 20

    ! END TEAM must restore the initial team.
    if (num_images() /= 4) error stop 21
    if (this_image() /= original_image) error stop 22

    sync all

    if (this_image() == 1) then
        print *, "CHANGE TEAM tests passed"
    end if

end program coarrays_34