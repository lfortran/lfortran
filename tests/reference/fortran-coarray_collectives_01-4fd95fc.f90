type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_collectives_01
implicit none
type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor
type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type
interface
    subroutine f__module_prif_prif_co_broadcast(a, source_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in) :: source_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_broadcast
end interface
interface
    subroutine f__module_prif_prif_co_broadcast_cptr(a_ptr, size_in_bytes, source_image, stat, errmsg, errmsg_alloc)
        type(c_ptr), intent(in) :: a_ptr
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(in) :: source_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_broadcast_cptr
end interface
interface
    subroutine f__module_prif_prif_co_max(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_max
end interface
interface
    subroutine f__module_prif_prif_co_max_character(a, result_image, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_max_character
end interface
interface
    subroutine f__module_prif_prif_co_min(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_min
end interface
interface
    subroutine f__module_prif_prif_co_min_character(a, result_image, stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_min_character
end interface
interface
    subroutine f__module_prif_prif_co_sum(a, result_image, stat, errmsg, errmsg_alloc)
        type(*), dimension(..), intent(inout), target :: a
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in), optional :: result_image
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_co_sum
end interface
interface
    subroutine f__module_prif_prif_init(stat)
        integer(4), intent(out) :: stat
    end subroutine f__module_prif_prif_init
end interface
interface
    subroutine f__module_prif_prif_num_images(num_images)
        integer(4), intent(out) :: num_images
    end subroutine f__module_prif_prif_num_images
end interface
interface
    subroutine f__module_prif_prif_stop(quiet, stop_code_int, stop_code_char)
        logical(1), intent(in) :: quiet
        character(len=*, kind=1), intent(in), optional :: stop_code_char
        integer(4), intent(in), optional :: stop_code_int
    end subroutine f__module_prif_prif_stop
end interface
interface
    subroutine f__module_prif_prif_sync_all(stat, errmsg, errmsg_alloc)
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_sync_all
end interface
interface
    subroutine f__module_prif_prif_this_image_no_coarray(team, this_image)
        import __module_prif_prif_team_type
        type(__module_prif_prif_team_type), intent(in), optional :: team
        integer(4), intent(out) :: this_image
    end subroutine f__module_prif_prif_this_image_no_coarray
end interface
type :: point
    real(4) :: x
    real(4) :: y
end type point
integer(4), dimension(:), allocatable :: arr
integer(4) :: me
integer(4) :: n_images
type(point) :: pt
character(len=2, kind=1), save :: str = "hi"
integer(4), dimension(:), allocatable :: v__co_broadcast_tmp
integer(4) :: val
me = lcompilers_prif_this_image()
n_images = lcompilers_prif_num_images()
call f__module_prif_prif_sync_all()
val = me
call f__module_prif_prif_co_sum(val)
call f__module_prif_prif_co_max(val)
call f__module_prif_prif_co_min(val)
call f__module_prif_prif_co_max_character(str)
call f__module_prif_prif_co_min_character(str)
call f__module_prif_prif_co_broadcast(val, 1)
call f__module_prif_prif_co_broadcast_cptr(c_loc(pt), 8_8, 1)
allocate(arr(5))
if (.not. is_contiguous(arr(1:5:2))) then
    allocate(v__co_broadcast_tmp(size(arr(1:5:2), 1)))
    v__co_broadcast_tmp = arr(1:5:2)
    call f__module_prif_prif_co_broadcast(v__co_broadcast_tmp, 1)
    arr(1:5:2) = v__co_broadcast_tmp
    deallocate(v__co_broadcast_tmp)
else
    call f__module_prif_prif_co_broadcast(arr(1:5:2), 1)
end if
call f__module_prif_prif_sync_all()
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call f__module_prif_prif_init(stat)
end subroutine f__lcompilers_collective_bootstrap

integer(4) function lcompilers_prif_num_images()
    call f__module_prif_prif_num_images(lcompilers_prif_num_images)
end function lcompilers_prif_num_images

integer(4) function lcompilers_prif_this_image()
    call f__module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_collectives_01
