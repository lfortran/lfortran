module coarray_saved_mod
implicit none
integer(4), save :: v__lcompilers_global_init_state_coarray_saved_mod = 0
integer(4), pointer :: w
integer(4), dimension(:), pointer :: x
integer(4), pointer, save :: y
integer(4), dimension(:), pointer, save :: z

contains

subroutine f__lcompilers_global_init_coarray_saved_mod()
    if (v__lcompilers_global_init_state_coarray_saved_mod /= 2) then
        if (v__lcompilers_global_init_state_coarray_saved_mod == 1) then
            error stop
        end if
        v__lcompilers_global_init_state_coarray_saved_mod = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarray_saved_mod_w__coarray_handle, v__module_coarray_saved_mod_w__coarray_data)
        call c_f_pointer(v__module_coarray_saved_mod_w__coarray_data, w)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__module_coarray_saved_mod_x__coarray_handle, v__module_coarray_saved_mod_x__coarray_data)
        call c_f_pointer(v__module_coarray_saved_mod_x__coarray_data, x, [10], [1])
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarray_saved_mod_y__coarray_handle, v__module_coarray_saved_mod_y__coarray_data)
        call c_f_pointer(v__module_coarray_saved_mod_y__coarray_data, y)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__module_coarray_saved_mod_z__coarray_handle, v__module_coarray_saved_mod_z__coarray_data)
        call c_f_pointer(v__module_coarray_saved_mod_z__coarray_data, z, [10], [1])
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_state_coarray_saved_mod = 2
    end if
end subroutine f__lcompilers_global_init_coarray_saved_mod

subroutine mod_sub()
end subroutine mod_sub

end module coarray_saved_mod

program coarray_saved_01
use coarray_saved_mod, only: mod_sub
use coarray_saved_mod, only: v__lcompilers_global_init_coarray_saved_mod => f__lcompilers_global_init_coarray_saved_mod
use coarray_saved_mod, only: w
use coarray_saved_mod, only: x
use coarray_saved_mod, only: y
use coarray_saved_mod, only: z
implicit none
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
    subroutine f__module_prif_prif_init(stat)
        integer(4), intent(out) :: stat
    end subroutine f__module_prif_prif_init
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
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface
call f__lcompilers_collective_bootstrap()
call v__lcompilers_global_init_coarray_saved_mod()
call f__lcompilers_global_init_tu_coarrays_coarray__bf1becf6c5f84fa6()
call mod_sub()
call coarray_saved_sub()
call f__module_prif_prif_stop(.false.)

contains

subroutine coarray_saved_sub()
end subroutine coarray_saved_sub

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call f__module_prif_prif_init(stat)
end subroutine f__lcompilers_collective_bootstrap

subroutine f__lcompilers_global_init_tu_coarrays_coarray__bf1becf6c5f84fa6()
    if (v__lcompilers_global_init_tu_coarrays_coarray__75af168d291c2e8c /= 2) then
        if (v__lcompilers_global_init_tu_coarrays_coarray__75af168d291c2e8c == 1) then
            error stop
        end if
        v__lcompilers_global_init_tu_coarrays_coarray__75af168d291c2e8c = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_a__coarray_handle,&
         v__cac_a__coarray_data)
        call c_f_pointer(v__cac_a__coarray_data, v__cac_a__coarray_ptr)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__cac_b__coarray_handle, v__cac_b__coarray_data)
        call c_f_pointer(v__cac_b__coarray_data, v__cac_b__coarray_ptr, [10], [1])
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_c__coarray_handle,&
         v__cac_c__coarray_data)
        call c_f_pointer(v__cac_c__coarray_data, v__cac_c__coarray_ptr)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__cac_d__coarray_handle, v__cac_d__coarray_data)
        call c_f_pointer(v__cac_d__coarray_data, v__cac_d__coarray_ptr, [10], [1])
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_w__coarray_handle,&
         v__cac_w__coarray_data)
        call c_f_pointer(v__cac_w__coarray_data, v__cac_w__coarray_ptr)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8*int(10, kind=8), null(),&
         v__cac_x__coarray_handle, v__cac_x__coarray_data)
        call c_f_pointer(v__cac_x__coarray_data, v__cac_x__coarray_ptr, [10], [1])
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_tu_coarrays_coarray__75af168d291c2e8c = 2
    end if
end subroutine f__lcompilers_global_init_tu_coarrays_coarray__bf1becf6c5f84fa6

end program coarray_saved_01
