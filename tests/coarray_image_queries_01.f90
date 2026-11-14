program coarray_image_queries_01
    use iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: team
    integer :: me, n, me_team, n_team, n_number

    me = this_image()
    n = num_images()

    form team (mod(me - 1, 2) + 1, team)

    change team (team)
        me_team = this_image(team)
        n_team = num_images(team=team)
    end team

    n_number = num_images(team_number=1)
    print *, me, n, me_team, n_team, n_number
end program
