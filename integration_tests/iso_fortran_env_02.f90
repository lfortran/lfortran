program iso_fortran_env_02
    use iso_fortran_env, only: team_type
    implicit none
    type(team_type) :: odd_even
    b: block
        print *, 'In block B'
        change team (odd_even)
        end team
        exit b
    end block b
    print *, "team_type available", len_trim("team_type")
end program iso_fortran_env_02
