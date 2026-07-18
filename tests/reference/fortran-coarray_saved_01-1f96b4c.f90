module coarray_saved_mod
implicit none
integer(4), pointer :: w
type(c_ptr) :: w__coarray_data
type(prif_coarray_handle) :: w__coarray_handle
integer(4), dimension(:), pointer :: x
type(c_ptr) :: x__coarray_data
type(prif_coarray_handle) :: x__coarray_handle
integer(4), pointer, save :: y
integer(4), dimension(:), pointer, save :: z

contains

subroutine mod_sub()
end subroutine mod_sub

end module coarray_saved_mod

type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarray_saved_01
use coarray_saved_mod, only: mod_sub
use coarray_saved_mod, only: w
use coarray_saved_mod, only: w__coarray_data
use coarray_saved_mod, only: w__coarray_handle
use coarray_saved_mod, only: x
use coarray_saved_mod, only: x__coarray_data
use coarray_saved_mod, only: x__coarray_handle
use coarray_saved_mod, only: y
use coarray_saved_mod, only: z
implicit none
integer(4), pointer, save :: a
integer(4), dimension(:), pointer, save :: b
integer(4), pointer :: c
type(c_ptr) :: c__coarray_data
type(prif_coarray_handle) :: c__coarray_handle
integer(4), dimension(:), pointer :: d
type(c_ptr) :: d__coarray_data
type(prif_coarray_handle) :: d__coarray_handle
integer(4) :: stat
call __module_prif_prif_init(stat)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), w__coarray_handle, w__coarray_data)
call c_f_pointer(w__coarray_data, w)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(), x__coarray_handle,&
         x__coarray_data)
call c_f_pointer(x__coarray_data, x, [10])
call c_f_pointer(y__coarray_data, y)
call c_f_pointer(z__coarray_data, z, [10])
call c_f_pointer(a__coarray_data, a)
call c_f_pointer(b__coarray_data, b, [10])
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), c__coarray_handle, c__coarray_data)
call c_f_pointer(c__coarray_data, c)
call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(), d__coarray_handle,&
         d__coarray_data)
call c_f_pointer(d__coarray_data, d, [10])
call __module_prif_prif_sync_all()
call mod_sub()
call coarray_saved_sub()
call __module_prif_prif_stop(.false.)

contains

subroutine __lfortran_coarray_init_coarray_saved_01_coarray_saved_mod_coarray_saved_sub()
    integer(4) :: stat
    call __module_prif_prif_init(stat)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), a__coarray_handle, a__coarray_data)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(), b__coarray_handle,&
         b__coarray_data)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), y__coarray_handle, y__coarray_data)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(), z__coarray_handle,&
         z__coarray_data)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), w__coarray_handle, w__coarray_data)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(), x__coarray_handle,&
         x__coarray_data)
end subroutine __lfortran_coarray_init_coarray_saved_01_coarray_saved_mod_coarray_saved_sub

interface
    subroutine __module_prif_prif_allocate_coarray(lcobounds, ucobounds, size_in_bytes, final_proc,&
        &
         coarray_handle, allocated_memory, stat, errmsg, errmsg_alloc)
        type(c_ptr), intent(out) :: allocated_memory
        type(prif_coarray_handle), intent(out) :: coarray_handle
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        procedure(prif_coarray_cleanup_interface), pointer, intent(in) :: final_proc
        integer(8), dimension(:), intent(in), value :: lcobounds
        integer(8), intent(in), value :: size_in_bytes
        integer(4), intent(out), optional :: stat
        integer(8), dimension(:), intent(in), value :: ucobounds
    end subroutine __module_prif_prif_allocate_coarray
end interface

interface
    subroutine __module_prif_prif_init(exit_code)
        integer(4), intent(out) :: exit_code
    end subroutine __module_prif_prif_init
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

subroutine coarray_saved_sub()
    integer(4), pointer, save :: w
    integer(4), dimension(:), pointer, save :: x
    call c_f_pointer(w__coarray_data, w)
    call c_f_pointer(x__coarray_data, x, [10])
end subroutine coarray_saved_sub

interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program coarray_saved_01
