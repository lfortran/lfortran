
type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor

type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type

type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_image_queries_01
implicit none
integer(4) :: me
integer(4) :: me_team
integer(4) :: n
integer(4) :: n_number
integer(4) :: n_team
integer(4) :: stat
type(__module_prif_prif_team_type) :: team
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
me = lcompilers_prif_this_image()
n = lcompilers_prif_num_images()
call __module_prif_prif_form_team(int(Mod(me - 1, 2) + 1, kind=8), team)
call __module_prif_prif_change_team(team)
me_team = lcompilers_prif_this_image_with_team(team)
n_team = lcompilers_prif_num_images_with_team(team)
call __module_prif_prif_end_team()
n_number = lcompilers_prif_num_images_with_team_number(int(1, kind=8))
print *, me, n, me_team, n_team, n_number
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
    subroutine __module_prif_prif_num_images(num_images)
        integer(4), intent(out) :: num_images
    end subroutine __module_prif_prif_num_images
end interface

interface
    subroutine __module_prif_prif_num_images_with_team(team, num_images)
        integer(4), intent(out) :: num_images
        type(__module_prif_prif_team_type), intent(in) :: team
    end subroutine __module_prif_prif_num_images_with_team
end interface

interface
    subroutine __module_prif_prif_num_images_with_team_number(team_number, num_images)
        integer(4), intent(out) :: num_images
        integer(8), intent(in) :: team_number
    end subroutine __module_prif_prif_num_images_with_team_number
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

integer(4) function lcompilers_prif_num_images()
    call __module_prif_prif_num_images(lcompilers_prif_num_images)
end function lcompilers_prif_num_images

integer(4) function lcompilers_prif_num_images_with_team(query_arg)
    type(__module_prif_prif_team_type), intent(in) :: query_arg
    call __module_prif_prif_num_images_with_team(query_arg, lcompilers_prif_num_images_with_team)
end function lcompilers_prif_num_images_with_team

integer(4) function lcompilers_prif_num_images_with_team_number(query_arg)
    integer(8), intent(in), value :: query_arg
    call __module_prif_prif_num_images_with_team_number(query_arg, lcompilers_prif_num_images_with_team_number)
end function lcompilers_prif_num_images_with_team_number

integer(4) function lcompilers_prif_this_image()
    call __module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

integer(4) function lcompilers_prif_this_image_with_team(query_arg)
    type(__module_prif_prif_team_type), intent(in) :: query_arg
    call __module_prif_prif_this_image_no_coarray(query_arg, lcompilers_prif_this_image_with_team)
end function lcompilers_prif_this_image_with_team

end program coarray_image_queries_01
