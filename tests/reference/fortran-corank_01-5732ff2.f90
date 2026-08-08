type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program corank_01
implicit none
integer(4), pointer :: a
type(c_ptr) :: a__coarray_data
type(prif_coarray_handle) :: a__coarray_handle
integer(4), pointer :: b
type(c_ptr) :: b__coarray_data
type(prif_coarray_handle) :: b__coarray_handle
integer(4), pointer :: c
type(c_ptr) :: c__coarray_data
type(prif_coarray_handle) :: c__coarray_handle
integer(4) :: stat
call __module_prif_prif_init(stat)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), a__coarray_handle, a__coarray_data)
call c_f_pointer(a__coarray_data, a)
call __module_prif_prif_allocate_coarray([1_8, 1_8], [2_8], 4_8, null(), b__coarray_handle, b__coarray_data)
call c_f_pointer(b__coarray_data, b)
call __module_prif_prif_allocate_coarray([1_8, 1_8, 1_8], [2_8, 3_8], 4_8, null(), c__coarray_handle, c__coarray_data)
call c_f_pointer(c__coarray_data, c)
call __module_prif_prif_sync_all()
print *, 1
print *, 2
print *, 3
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
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program corank_01
