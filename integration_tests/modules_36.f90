module fpm_main_01

type :: fpm_build_settings
    logical :: list=.false.
end type

type, extends(fpm_build_settings) :: fpm_run_settings
    character(len=5), allocatable :: name(:)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: runner
    logical :: example
end type

contains

subroutine cmd_run(settings)
    class(fpm_run_settings), intent(in) :: settings
    logical :: found(size(settings%name))
    logical :: toomany
    logical, intent(in) :: test

    if ( any(.not.found) &
    & .or. &
    & ( (toomany .and. .not.test) .or.  (toomany .and. settings%runner /= '') ) &
    & .and. &
    & .not.settings%list) then
    end if

end subroutine cmd_run

end module fpm_main_01
