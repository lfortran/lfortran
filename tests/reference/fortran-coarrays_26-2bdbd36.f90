module coarrays_26_m
implicit none
integer(4), save :: v__lcompilers_global_init_state_coarrays_26_m = 0
integer(4), pointer :: x

contains

subroutine f__lcompilers_global_init_coarrays_26_m()
    if (v__lcompilers_global_init_state_coarrays_26_m /= 2) then
        if (v__lcompilers_global_init_state_coarrays_26_m == 1) then
            error stop
        end if
        v__lcompilers_global_init_state_coarrays_26_m = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarrays_26_m_x__coarray_handle, v__module_coarrays_26_m_x__coarray_data)
        call c_f_pointer(v__module_coarrays_26_m_x__coarray_data, x)
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_state_coarrays_26_m = 2
    end if
end subroutine f__lcompilers_global_init_coarrays_26_m

end module coarrays_26_m

module coarrays_26_m2
implicit none
integer(4), save :: v__lcompilers_global_init_state_coarrays_26_m2 = 0
integer(4), pointer :: x

contains

subroutine f__lcompilers_global_init_coarrays_26_m2()
    if (v__lcompilers_global_init_state_coarrays_26_m2 /= 2) then
        if (v__lcompilers_global_init_state_coarrays_26_m2 == 1) then
            error stop
        end if
        v__lcompilers_global_init_state_coarrays_26_m2 = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarrays_26_m2_x__coarray_handle, v__module_coarrays_26_m2_x__coarray_data)
        call c_f_pointer(v__module_coarrays_26_m2_x__coarray_data, x)
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_state_coarrays_26_m2 = 2
    end if
end subroutine f__lcompilers_global_init_coarrays_26_m2

end module coarrays_26_m2

module coarrays_26_m3
implicit none
integer(4), save :: v__lcompilers_global_init_state_coarrays_26_m3 = 0
integer(4), pointer :: x

contains

subroutine coarrays_26_mod_sub()
    v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarra&
        y_ptr + 1
    call coarrays_26_mod_inner()
    if (v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr /= 11) then
        error stop
    end if
    call coarrays_26_mod_host()
    if (v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr /= 12) then
        error stop
    end if
    contains
    subroutine coarrays_26_mod_host()
        v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = v__module_coarrays_26_m3_coarrays_26_mod_sub_x__co&
        array_ptr + 1
    end subroutine coarrays_26_mod_host

    subroutine coarrays_26_mod_inner()
        v__module_coarrays_26_m3_coarrays_26_mod_sub_c_a2ebe4d0d0ebcfb1 = v__module_coarrays_26_m3_coarrays_26_mod_sub_c&
        _a2ebe4d0d0ebcfb1 + 1
        if (v__module_coarrays_26_m3_coarrays_26_mod_sub_c_a2ebe4d0d0ebcfb1 /= 21) then
            error stop
        end if
    end subroutine coarrays_26_mod_inner

end subroutine coarrays_26_mod_sub

subroutine f__lcompilers_global_init_coarrays_26_m3()
    if (v__lcompilers_global_init_state_coarrays_26_m3 /= 2) then
        if (v__lcompilers_global_init_state_coarrays_26_m3 == 1) then
            error stop
        end if
        v__lcompilers_global_init_state_coarrays_26_m3 = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarrays_26_m3_x__coarray_handle, v__module_coarrays_26_m3_x__coarray_data)
        call c_f_pointer(v__module_coarrays_26_m3_x__coarray_data, x)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_handle,&
         v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_data)
        call c_f_pointer(v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_data, v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr)
        v__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = 10
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         v__module_coarrays_26_m3_coarrays_26_mod_sub_c_2770d444e2f98ddb,&
         v__module_coarrays_26_m3_coarrays_26_mod_sub_c_11b0ee38a2b99c3d)
        call c_f_pointer(v__module_coarrays_26_m3_coarrays_26_mod_sub_c_11b0ee38a2b99c3d, v__module_coarrays_26_m3_coarrays_26_mod_sub_c_a2ebe4d0d0ebcfb1)
        v__module_coarrays_26_m3_coarrays_26_mod_sub_c_a2ebe4d0d0ebcfb1 = 20
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_state_coarrays_26_m3 = 2
    end if
end subroutine f__lcompilers_global_init_coarrays_26_m3

end module coarrays_26_m3

program coarrays_26
use coarrays_26_m3, only: coarrays_26_mod_sub
use coarrays_26_m, only: module_x => x
use coarrays_26_m2, only: module_x2 => x
use coarrays_26_m3, only: module_x3 => x
use coarrays_26_m, only: v__lcompilers_global_init_coarrays_26_m => f__lcompilers_global_init_coarrays_26_m
use coarrays_26_m2, only: v__lcompilers_global_init_coarrays_26_m2 => f__lcompilers_global_init_coarrays_26_m2
use coarrays_26_m3, only: v__lcompilers_global_init_coarrays_26_m3 => f__lcompilers_global_init_coarrays_26_m3
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
    subroutine f__module_prif_prif_this_image_no_coarray(team, this_image)
        import __module_prif_prif_team_type
        type(__module_prif_prif_team_type), intent(in), optional :: team
        integer(4), intent(out) :: this_image
    end subroutine f__module_prif_prif_this_image_no_coarray
end interface
interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        import prif_coarray_handle
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface
integer(4), save :: x__coarray_ptr = 7
call f__lcompilers_collective_bootstrap()
call v__lcompilers_global_init_coarrays_26_m()
call v__lcompilers_global_init_coarrays_26_m2()
call v__lcompilers_global_init_coarrays_26_m3()
call f__lcompilers_global_init_tu_coarrays_coarrays_8c7a049c685a83d6()
module_x = lcompilers_prif_this_image()
module_x2 = lcompilers_prif_this_image() + 1
module_x3 = lcompilers_prif_this_image() + 2
v__cac_x__coarray_ptr = lcompilers_prif_this_image()*10
call coarrays_26_sub()
call coarrays_26_mod_sub()
call coarrays_26_prog_inner()
call f__module_prif_prif_sync_all()
if (v__cac_x__coarray_ptr /= lcompilers_prif_this_image()*10) then
    error stop
end if
if (x__coarray_ptr /= 7) then
    error stop
end if
if (module_x /= lcompilers_prif_this_image()) then
    error stop
end if
if (module_x2 /= lcompilers_prif_this_image() + 1) then
    error stop
end if
if (module_x3 /= lcompilers_prif_this_image() + 2) then
    error stop
end if
call f__module_prif_prif_stop(.false.)

contains

subroutine coarrays_26_prog_inner()
    v__cac_x__coarray_ptr1 = v__cac_x__coarray_ptr1 + 1
    if (v__cac_x__coarray_ptr1 /= 41) then
        error stop
    end if
end subroutine coarrays_26_prog_inner

subroutine coarrays_26_sub()
    integer(4) :: x__coarray_data
    integer(4) :: x__coarray_handle
    integer(4) :: x__coarray_ptr
    x__coarray_ptr = -1
    x__coarray_handle = -2
    x__coarray_data = -3
    v__cac_x__coarray_ptr2 = lcompilers_prif_this_image() + 100
    call coarrays_26_sub2()
    call coarrays_26_inner()
    call coarrays_26_host()
    if (v__cac_x__coarray_ptr2 /= lcompilers_prif_this_image() + 101) then
        error stop
    end if
    if (x__coarray_ptr /= (-1) .or. x__coarray_handle /= (-2) .or. x__coarray_data /= (-3)) then
        error stop
    end if
    contains
    subroutine coarrays_26_host()
        v__cac_x__coarray_ptr2 = v__cac_x__coarray_ptr2 + 1
    end subroutine coarrays_26_host

    subroutine coarrays_26_inner()
        v__cac_x__coarray_ptr3 = v__cac_x__coarray_ptr3 + 1
        if (v__cac_x__coarray_ptr3 /= 31) then
            error stop
        end if
    end subroutine coarrays_26_inner

end subroutine coarrays_26_sub

subroutine coarrays_26_sub2()
    v__cac_x__coarray_ptr4 = lcompilers_prif_this_image() + 1000
    if (v__cac_x__coarray_ptr4 /= lcompilers_prif_this_image() + 1000) then
        error stop
    end if
end subroutine coarrays_26_sub2

subroutine f__lcompilers_collective_bootstrap()
    integer(4) :: stat
    call f__module_prif_prif_init(stat)
end subroutine f__lcompilers_collective_bootstrap

subroutine f__lcompilers_global_init_tu_coarrays_coarrays_8c7a049c685a83d6()
    if (v__lcompilers_global_init_tu_coarrays_coarrays_f7c1f6a44d1e899c /= 2) then
        if (v__lcompilers_global_init_tu_coarrays_coarrays_f7c1f6a44d1e899c == 1) then
            error stop
        end if
        v__lcompilers_global_init_tu_coarrays_coarrays_f7c1f6a44d1e899c = 1
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_x__coarray_handle,&
         v__cac_x__coarray_data)
        call c_f_pointer(v__cac_x__coarray_data, v__cac_x__coarray_ptr)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_x__coarray_handle1,&
         v__cac_x__coarray_data1)
        call c_f_pointer(v__cac_x__coarray_data1, v__cac_x__coarray_ptr1)
        v__cac_x__coarray_ptr1 = 40
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_x__coarray_handle2,&
         v__cac_x__coarray_data2)
        call c_f_pointer(v__cac_x__coarray_data2, v__cac_x__coarray_ptr2)
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_x__coarray_handle3,&
         v__cac_x__coarray_data3)
        call c_f_pointer(v__cac_x__coarray_data3, v__cac_x__coarray_ptr3)
        v__cac_x__coarray_ptr3 = 30
        call f__module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), v__cac_x__coarray_handle4,&
         v__cac_x__coarray_data4)
        call c_f_pointer(v__cac_x__coarray_data4, v__cac_x__coarray_ptr4)
        call f__module_prif_prif_sync_all()
        v__lcompilers_global_init_tu_coarrays_coarrays_f7c1f6a44d1e899c = 2
    end if
end subroutine f__lcompilers_global_init_tu_coarrays_coarrays_8c7a049c685a83d6

integer(4) function lcompilers_prif_this_image()
    call f__module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

end program coarrays_26
