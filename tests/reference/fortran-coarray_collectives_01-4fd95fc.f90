type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor

type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type

type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle


program coarray_collectives_01
implicit none
integer(4) :: me
integer(4) :: n_images
integer(4) :: stat
character(len=2, kind=1), save :: str = "hi"
integer(4) :: val
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
me = lcompilers_prif_this_image()
n_images = lcompilers_prif_num_images()
call __module_prif_prif_sync_all()
val = me
call __module_prif_prif_co_sum(val)
call __module_prif_prif_co_max(val)
call __module_prif_prif_co_min(val)
call __module_prif_prif_co_max_character(str)
call __module_prif_prif_co_min_character(str)
call __module_prif_prif_co_broadcast(val, 1)
call __module_prif_prif_sync_all()
call __module_prif_prif_stop(.false.)

contains

interface
    subroutine __module_prif_prif_co_broadcast(a, source_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in) :: source_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_broadcast
end interface

interface
    subroutine __module_prif_prif_co_max(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_max
end interface

interface
    subroutine __module_prif_prif_co_max_character(a, result_image, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_max_character
end interface

interface
    subroutine __module_prif_prif_co_min(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_min
end interface

interface
    subroutine __module_prif_prif_co_min_character(a, result_image, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_min_character
end interface

interface
    subroutine __module_prif_prif_co_sum(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_co_sum
end interface

interface
    subroutine __module_prif_prif_init(exit_code)
        integer(4), intent(out) :: exit_code
    end subroutine __module_prif_prif_init
end interface

interface
    subroutine __module_prif_prif_num_images(num_images)
        integer(4), intent(out) :: num_images
    end subroutine __module_prif_prif_num_images
end interface

interface
    subroutine __module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in), value :: quiet
        character(len=*, kind=1), intent(in), optional, value :: stop_code_char
        integer(4), intent(in), optional, value :: stop_code_int
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

integer(4) function lcompilers_prif_this_image()
    call __module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_collectives_01
