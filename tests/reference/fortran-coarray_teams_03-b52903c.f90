
type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor

type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type

type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_teams_03
implicit none
integer(4), dimension(4) :: expected_image
integer(4), dimension(4) :: expected_team
integer(4) :: old_image
integer(4) :: stat
type(__module_prif_prif_team_type) :: team
integer(4) :: team_num
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
expected_team = [1, 1, 2, 2]
expected_image = [1, 2, 1, 2]
old_image = lcompilers_prif_this_image()
team_num = (old_image - 1)/2 + 1
call __module_prif_prif_form_team(int(team_num, kind=8), team)
call __module_prif_prif_change_team(team)
if (team_num /= expected_team(old_image)) then
    error stop
end if
if (lcompilers_prif_this_image() /= expected_image(old_image)) then
    error stop
end if
call __module_prif_prif_end_team()
call __module_prif_prif_stop(.false.)

contains

interface
    subroutine __module_prif_prif_change_team(team, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
        type(__module_prif_prif_team_type), intent(in) :: team
    end subroutine __module_prif_prif_change_team
end interface

interface
    subroutine __module_prif_prif_end_team(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_end_team
end interface

interface
    subroutine __module_prif_prif_form_team(team_number, team, new_index, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: new_index
        integer(4), intent(out), optional :: stat
        type(__module_prif_prif_team_type), intent(out) :: team
        integer(8), intent(in) :: team_number
    end subroutine __module_prif_prif_form_team
end interface

interface
    subroutine __module_prif_prif_init(stat)
        integer(4), intent(out) :: stat
    end subroutine __module_prif_prif_init
end interface

interface
    subroutine __module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in) :: quiet
        character(len=*, kind=1), intent(in), optional :: stop_code_char
        integer(4), intent(in), optional :: stop_code_int
    end subroutine __module_prif_prif_stop
end interface

interface
    subroutine __module_prif_prif_sync_all(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_sync_all
end interface

interface
    subroutine __module_prif_prif_this_image_no_coarray(team, this_image)
        type(__module_prif_prif_team_type), intent(in), optional :: team
        integer(4), intent(out) :: this_image
    end subroutine __module_prif_prif_this_image_no_coarray
end interface

integer(4) function lcompilers_prif_this_image()
    call __module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_teams_03
