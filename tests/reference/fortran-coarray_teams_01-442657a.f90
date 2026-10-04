
type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_teams_01
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
    subroutine f__module_prif_prif_init(stat)
        integer(4), intent(out) :: stat
    end subroutine f__module_prif_prif_init
end interface
interface
    subroutine f__module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in) :: quiet
        character(len=*, kind=1), intent(in), optional :: stop_code_char
        integer(4), intent(in), optional :: stop_code_int
    end subroutine f__module_prif_prif_stop
end interface
interface
    subroutine f__module_prif_prif_this_image_no_coarray(team, this_image)
        import __module_prif_prif_team_type
        type(__module_prif_prif_team_type), intent(in), optional :: team
        integer(4), intent(out) :: this_image
    end subroutine f__module_prif_prif_this_image_no_coarray
end interface
character(len=64, kind=1) :: errmsg
integer(4) :: iostat
integer(4) :: new_index
type(__module_prif_prif_team_type) :: team1
type(__module_prif_prif_team_type) :: team2
integer(4) :: team_num
team_num = Mod(lcompilers_prif_this_image() - 1, 2) + 1
call f__module_prif_prif_form_team(int(1, kind=8), team1)
call f__module_prif_prif_form_team(int(team_num, kind=8), team2)
call f__module_prif_prif_form_team(int(1, kind=8), team1, 1)
call f__module_prif_prif_form_team(int(1, kind=8), team1, iostat)
call f__module_prif_prif_form_team(int(1, kind=8), team1, errmsg)
call f__module_prif_prif_form_team(int(team_num, kind=8), team2, (lcompilers_prif_this_image() - 1)/2 + 1, iostat,&
         errmsg)
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call f__module_prif_prif_init(stat)
end subroutine f__lcompilers_collective_bootstrap

integer(4) function lcompilers_prif_this_image()
    call f__module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_teams_01
