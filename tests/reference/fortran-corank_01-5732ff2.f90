type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program corank_01
implicit none
type :: t
    integer(4) :: a
    integer(4) :: b
end type t
integer(4) :: c1
integer(4) :: c2
integer(4) :: c3
integer(4) :: c4
integer(4) :: c5
type(t) :: s
integer(4) :: stat
integer(4), pointer :: x
type(c_ptr) :: x__coarray_data
type(prif_coarray_handle) :: x__coarray_handle
integer(4), dimension(:), pointer :: y
type(c_ptr) :: y__coarray_data
type(prif_coarray_handle) :: y__coarray_handle
integer(4), pointer :: z
type(c_ptr) :: z__coarray_data
type(prif_coarray_handle) :: z__coarray_handle
call __module_prif_prif_init(stat)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle, x__coarray_data)
call c_f_pointer(x__coarray_data, x)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(5, kind=8), null(), y__coarray_handle,&
         y__coarray_data)
call c_f_pointer(y__coarray_data, y, [5], [1])
call __module_prif_prif_allocate_coarray([1_8, 1_8, 1_8], [2_8, 3_8], 4_8, null(), z__coarray_handle, z__coarray_data)
call c_f_pointer(z__coarray_data, z)
call __module_prif_prif_sync_all()
c1 = 1
c2 = 1
c3 = 3
c4 = 1
c5 = 2
c1 = 1
c2 = 1
c3 = 3
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
