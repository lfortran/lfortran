module modules_36_fpm_main_01

type :: fpm_build_settings
    logical :: list=.false.
end type

type, extends(fpm_build_settings) :: fpm_run_settings
    ! TODO: Set allocatable attribute after supporting
    ! allocation of StructInstanceMember
    character(len=5) :: name(2)
    character(len=4) :: args
    character(len=6) :: runner
    logical :: example
end type

contains

subroutine cmd_run(settings, test)
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

end module modules_36_fpm_main_01

program modules_36
use modules_36_fpm_main_01
implicit none

type(fpm_run_settings) :: settings
! TODO: Re-enable after supporting allocation of StructInstanceMember
! allocate(settings%name(2))
! allocate(character(len=4)::settings%args)
! allocate(character(len=6)::settings%runner)
call cmd_run(settings, .true.)

end program
