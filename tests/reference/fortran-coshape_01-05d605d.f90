type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coshape_01
implicit none
integer(4) :: stat
integer(4), pointer :: x
type(c_ptr) :: x__coarray_data
type(prif_coarray_handle) :: x__coarray_handle
call __module_prif_prif_init(stat)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle, x__coarray_data)
call c_f_pointer(x__coarray_data, x)
call __module_prif_prif_sync_all()
if (Any(lcompilers_prif_coshape_corank1_k8(x__coarray_handle) /= [2_8])) then
    error stop
end if
call __module_prif_prif_stop(.false.)

contains

interface
    subroutine __module_prif_prif_allocate_coarray(lcobounds, ucobounds, size_in_bytes, final_proc,&
        &
         coarray_handle, allocated_memory, stat, errmsg, errmsg_alloc)
        type(c_ptr), intent(out) :: allocated_memory
        type(prif_coarray_handle), intent(out) :: coarray_handle
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        procedure(prif_coarray_cleanup_interface), pointer, intent(in) :: final_proc
        integer(8), dimension(:), intent(in) :: lcobounds
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(out), optional :: stat
        integer(8), dimension(:), intent(in) :: ucobounds
    end subroutine __module_prif_prif_allocate_coarray
end interface

interface
    subroutine __module_prif_prif_coshape(coarray_handle, sizes)
        type(prif_coarray_handle), intent(in) :: coarray_handle
        integer(8), dimension(:), intent(out) :: sizes
    end subroutine __module_prif_prif_coshape
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

function lcompilers_prif_coshape_corank1_k8(coarray_handle) result(result)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(8), dimension(1), value :: result
    integer(8), dimension(int(1, kind=8):(int(1, kind=8))+(int(1, kind=8))-1) :: sizes
    call __module_prif_prif_coshape(coarray_handle, sizes)
    result = int(sizes, kind=8)
end function lcompilers_prif_coshape_corank1_k8

interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program coshape_01
