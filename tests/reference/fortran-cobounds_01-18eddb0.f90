type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program cobounds_01
implicit none
integer(4) :: a
integer(4) :: b
integer(4), dimension(1) :: lc
integer(4) :: stat
integer(4), dimension(1) :: uc
integer(4), dimension(:), pointer :: x
type(c_ptr) :: x__coarray_data
type(prif_coarray_handle) :: x__coarray_handle
call __module_prif_prif_init(stat)
call __module_prif_prif_allocate_coarray([int(2, kind=8)], [integer(8) :: ], 4_8*int(5, kind=8), null(),&
         x__coarray_handle, x__coarray_data)
call c_f_pointer(x__coarray_data, x, [5])
call __module_prif_prif_sync_all()
a = lcompilers_prif_lcobound_with_dim_k4(x__coarray_handle, 1)
b = lcompilers_prif_ucobound_with_dim_k4(x__coarray_handle, 1)
lc = [lcompilers_prif_lcobound_with_dim_k4(x__coarray_handle, 1)]
uc = [lcompilers_prif_ucobound_with_dim_k4(x__coarray_handle, 1)]
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
    subroutine __module_prif_prif_lcobound_with_dim(coarray, dim, lcobound)
        type(prif_coarray_handle), intent(in) :: coarray
        integer(4), intent(in) :: dim
        integer(8), intent(out) :: lcobound
    end subroutine __module_prif_prif_lcobound_with_dim
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
    subroutine __module_prif_prif_ucobound_with_dim(coarray, dim, ucobound)
        type(prif_coarray_handle), intent(in) :: coarray
        integer(4), intent(in) :: dim
        integer(8), intent(out) :: ucobound
    end subroutine __module_prif_prif_ucobound_with_dim
end interface

integer(4) function lcompilers_prif_lcobound_with_dim_k4(coarray_ptr, dim_val)
    type(prif_coarray_handle), intent(in) :: coarray_ptr
    integer(4), intent(in), value :: dim_val
    integer(8) :: sub_res
    call __module_prif_prif_lcobound_with_dim(coarray_ptr, dim_val, sub_res)
    lcompilers_prif_lcobound_with_dim_k4 = int(sub_res, kind=4)
end function lcompilers_prif_lcobound_with_dim_k4

integer(4) function lcompilers_prif_ucobound_with_dim_k4(coarray_ptr, dim_val)
    type(prif_coarray_handle), intent(in) :: coarray_ptr
    integer(4), intent(in), value :: dim_val
    integer(8) :: sub_res
    call __module_prif_prif_ucobound_with_dim(coarray_ptr, dim_val, sub_res)
    lcompilers_prif_ucobound_with_dim_k4 = int(sub_res, kind=4)
end function lcompilers_prif_ucobound_with_dim_k4

interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program cobounds_01
