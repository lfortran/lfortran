program coarray_allocate_01
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
    subroutine f__module_prif_prif_deallocate_coarray(coarray_handle, stat, errmsg, errmsg_alloc)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in) :: coarray_handle
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(out), optional :: stat
    end subroutine f__module_prif_prif_deallocate_coarray
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
integer(4), pointer :: a
integer(4), dimension(:), pointer, save :: b
character(len=100, kind=1) :: errmsg
integer(4) :: stat
integer(4), pointer :: x
call f__module_prif_prif_allocate_coarray([int(1, kind=8)], [integer(8) :: ], 4_8, null(), v__cac_a__coarray_handle,&
         v__cac_a__coarray_data, stat, errmsg)
call c_f_pointer(v__cac_a__coarray_data, a)
call f__module_prif_prif_allocate_coarray([int(1, kind=8)], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__cac_b__coarray_handle, v__cac_b__coarray_data, stat, errmsg)
call c_f_pointer(v__cac_b__coarray_data, b, [10], [1])
call f__module_prif_prif_allocate_coarray([int(1, kind=8), int(1, kind=8)], [int(2, kind=8)], 4_8, null(),&
         v__cac_x__coarray_handle, v__cac_x__coarray_data, stat, errmsg)
call c_f_pointer(v__cac_x__coarray_data, x)
a = lcompilers_prif_this_image()
x = lcompilers_prif_this_image()
call f__module_prif_prif_deallocate_coarray(v__cac_a__coarray_handle)
nullify (a)
call f__module_prif_prif_deallocate_coarray(v__cac_b__coarray_handle)
nullify (b)
call f__module_prif_prif_deallocate_coarray(v__cac_x__coarray_handle)
nullify (x)
call f__module_prif_prif_stop(.false.)

contains

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call lcompilers_prif_start(stat)
    if (stat /= 0) then
        error stop
    end if
end subroutine f__lcompilers_collective_bootstrap

integer(4) function lcompilers_prif_this_image()
    call f__module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarray_allocate_01
