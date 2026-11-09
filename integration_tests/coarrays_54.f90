program coarrays_54
    ! THIS_IMAGE(TEAM), NUM_IMAGES(TEAM) and NUM_IMAGES(TEAM_NUMBER), lowered
    ! to prif_this_image_no_coarray, prif_num_images_with_team and
    ! prif_num_images_with_team_number.
    !
    ! Requires exactly 4 images. Original images 1, 2 and 3 form team 1 and
    ! image 4 forms team 2, so that the two teams differ in size. Every query
    ! names the current team, an ancestor team or the initial team (team
    ! number -1), as the standard requires.
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team1
    type(team_type) :: team2
    integer :: original_image
    integer :: team_num
    integer :: team1_size
    integer :: team1_image
    integer(8) :: initial_team_number

    original_image = this_image()
    if (num_images() /= 4) error stop 1

    if (original_image <= 3) then
        team_num = 1
        team1_size = 3
    else
        team_num = 2
        team1_size = 1
    end if

    ! TEAM_NUMBER of the initial team: positional, keyword, other kinds and
    ! an expression that itself contains an image query.
    if (num_images(-1) /= 4) error stop 2
    if (num_images(team_number=-1) /= 4) error stop 3
    initial_team_number = -1
    if (num_images(initial_team_number) /= 4) error stop 4
    if (num_images(team_number=-1_2) /= 4) error stop 5
    if (num_images(team_number=-(this_image()/this_image())) /= 4) error stop 6

    form team(team_num, team1)

    change team(team1)

        ! TEAM of the current team, positional and keyword.
        if (num_images(team1) /= team1_size) error stop 7
        if (num_images(team=team1) /= team1_size) error stop 8
        if (num_images(team1) /= num_images()) error stop 9

        if (this_image(team1) /= this_image()) error stop 10
        if (this_image(team=team1) /= this_image()) error stop 11
        if (this_image(team1) < 1 .or. this_image(team1) > team1_size) error stop 12

        ! TEAM_NUMBER of the current team and of the initial team.
        if (num_images(team_number=team_num) /= team1_size) error stop 13
        if (num_images(team_number=-1) /= 4) error stop 14

        team1_image = this_image()
        sync all

        ! Put every image in a team of its own.
        form team(this_image(), team2)

        change team(team2)

            if (num_images() /= 1) error stop 15
            if (this_image() /= 1) error stop 16
            if (num_images(team2) /= 1) error stop 17
            if (this_image(team2) /= 1) error stop 18

            ! TEAM of an ancestor team.
            if (num_images(team1) /= team1_size) error stop 19
            if (this_image(team1) /= team1_image) error stop 20
            if (this_image(team=team1) /= team1_image) error stop 21

            if (num_images(team_number=-1) /= 4) error stop 22

        end team

        if (num_images(team1) /= team1_size) error stop 23
        if (this_image(team1) /= team1_image) error stop 24

        sync all

    end team

    if (num_images() /= 4) error stop 25
    if (this_image() /= original_image) error stop 26
    if (num_images(team_number=-1) /= 4) error stop 27

    sync all

    if (this_image() == 1) then
        print *, "THIS_IMAGE(TEAM) and NUM_IMAGES(TEAM | TEAM_NUMBER) tests passed"
    end if

end program coarrays_54
