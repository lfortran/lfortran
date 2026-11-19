module getenv_01_m
    implicit none
contains
    function env_value(name) result(d)
        character(len=*), intent(in) :: name
        character(len=256) :: d
        ! GNU/F77 extension, same as get_environment_variable(name, d)
        call getenv(name, d)
    end function env_value
end module getenv_01_m

program getenv_01
    use getenv_01_m
    implicit none
    character(len=256) :: ref, val

    ! An existing variable must give the same result as the standard intrinsic
    call get_environment_variable('PATH', ref)
    call getenv('PATH', val)
    if (val /= ref) error stop "getenv('PATH') differs from get_environment_variable"
    if (len_trim(val) == 0) error stop "PATH should be set"
    if (env_value('PATH') /= ref) error stop "getenv inside a function differs"

    ! A missing variable yields a blank value
    val = 'garbage'
    call getenv('LFORTRAN_GETENV_01_NONEXISTENT_VAR', val)
    if (len_trim(val) /= 0) error stop "missing variable should give blanks"

    ! Variable name given as an expression
    val = ''
    call getenv(trim('PATH '), val)
    if (val /= ref) error stop "getenv with trimmed name differs"
end program getenv_01
