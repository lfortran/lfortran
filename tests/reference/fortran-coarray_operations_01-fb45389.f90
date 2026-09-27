program coarray_operations_01
implicit none
type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor
type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type
type, bind(c) :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle
interface
    subroutine f__module_prif_prif_allocate_coarray(lcobounds, ucobounds, size_in_bytes, final_proc, coarray_handle,&
         allocated_memory, stat, errmsg, errmsg_alloc)
        import prif_coarray_handle
        type(c_ptr), intent(out) :: allocated_memory
        type(prif_coarray_handle), intent(out) :: coarray_handle
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        procedure(prif_coarray_cleanup_interface), pointer, intent(in) :: final_proc
        integer(8), dimension(:), intent(in) :: lcobounds
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(out), optional :: stat
        integer(8), dimension(:), intent(in) :: ucobounds
    end subroutine f__module_prif_prif_allocate_coarray
end interface
interface
    subroutine f__module_prif_prif_get(image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, stat,&
         errmsg, errmsg_alloc)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in) :: coarray_handle
        type(c_ptr), intent(in) :: current_image_buffer
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in) :: image_num
        integer(8), intent(in) :: offset
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_get
end interface
interface
    subroutine f__module_prif_prif_initial_team_index(coarray_handle, sub, initial_team_index, stat)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in) :: coarray_handle
        integer(4), intent(out) :: initial_team_index
        integer(4), intent(out), optional :: stat
        integer(8), dimension(:), intent(in) :: sub
    end subroutine f__module_prif_prif_initial_team_index
end interface
interface
    subroutine f__module_prif_prif_put(image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, stat,&
         errmsg, errmsg_alloc)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in) :: coarray_handle
        type(c_ptr), intent(in) :: current_image_buffer
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in) :: image_num
        integer(8), intent(in) :: offset
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_put
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
interface
    subroutine lcompilers_prif_start(stat) bind(c, name = "lcompilers_prif_start")
        integer(4), intent(out) :: stat
    end subroutine lcompilers_prif_start
end interface
interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface
integer(4) :: me
me = lcompilers_prif_this_image()
v__cac_a__coarray_ptr = me
call f__module_prif_prif_sync_all()
if (me == 1) then
    v__cac_a__coarray_ptr = lcompilers_prif_get_integer(4)(v__cac_a__coarray_handle, [int(2, kind=8)], int(0, kind=8))
    call lcompilers_prif_put_integer(4)(v__cac_a__coarray_handle, [int(2, kind=8)], int(0, kind=8), me)
end if
call f__module_prif_prif_sync_all()
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call lcompilers_prif_start(stat)
    if (stat /= 0) then
        error stop
    end if
end subroutine f__lcompilers_collective_bootstrap

subroutine f__lcompilers_global_init_tu_coarrays_coarray_operations_01()
    call _lcompilers_init_require_collective()
    call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_a__coarray_handle,&
         v__cac_a__coarray_data)
    call c_f_pointer(v__cac_a__coarray_data, v__cac_a__coarray_ptr)
    call f__module_prif_prif_sync_all()
end subroutine f__lcompilers_global_init_tu_coarrays_coarray_operations_01

integer(4) function lcompilers_prif_get_integer(4)(coarray_handle, sub, offset) result(result)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(4) :: image_num
    integer(8), intent(in), value :: offset
    integer(8), dimension(:), intent(in), value :: sub
    call f__module_prif_prif_initial_team_index(coarray_handle, sub, image_num)
    call f__module_prif_prif_get(image_num, coarray_handle, offset, c_loc(result), 4_8)
end function lcompilers_prif_get_integer(4)

subroutine lcompilers_prif_put_integer(4)(coarray_handle, sub, offset, value)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(4) :: image_num
    integer(8), intent(in), value :: offset
    integer(8), dimension(:), intent(in), value :: sub
    integer(4), intent(in), value :: value
    call f__module_prif_prif_initial_team_index(coarray_handle, sub, image_num)
    call f__module_prif_prif_put(image_num, coarray_handle, offset, c_loc(value), 4_8)
end subroutine lcompilers_prif_put_integer(4)

integer(4) function lcompilers_prif_this_image()
    call f__module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_operations_01
