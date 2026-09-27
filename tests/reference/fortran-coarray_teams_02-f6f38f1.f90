
type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_teams_02
implicit none
type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor
type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type
interface
    subroutine f__module_prif_prif_form_team(team_number, team, new_index, stat, errmsg, errmsg_alloc)
        import __module_prif_prif_team_type
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: new_index
        integer(4), intent(out), optional :: stat
        type(__module_prif_prif_team_type), intent(out) :: team
        integer(8), intent(in) :: team_number
    end subroutine f__module_prif_prif_form_team
end interface
interface
    subroutine f__module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in) :: quiet
        character(len=*, kind=1), intent(in), optional :: stop_code_char
        integer(4), intent(in), optional :: stop_code_int
    end subroutine f__module_prif_prif_stop
end interface
interface
    subroutine f__module_prif_prif_sync_team(team, stat, errmsg, errmsg_alloc)
        import __module_prif_prif_team_type
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
        type(__module_prif_prif_team_type), intent(in) :: team
    end subroutine f__module_prif_prif_sync_team
end interface
interface
    subroutine lcompilers_prif_start(stat) bind(c, name = "lcompilers_prif_start")
        integer(4), intent(out) :: stat
    end subroutine lcompilers_prif_start
end interface
type(__module_prif_prif_team_type) :: team
call f__module_prif_prif_form_team(int(1, kind=8), team)
call f__module_prif_prif_sync_team(team)
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call lcompilers_prif_start(stat)
    if (stat /= 0) then
        error stop
    end if
end subroutine f__lcompilers_collective_bootstrap

end program coarray_teams_02
