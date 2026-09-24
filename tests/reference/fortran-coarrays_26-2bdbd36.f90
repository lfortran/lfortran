module coarrays_26_m
implicit none
integer(4), pointer :: x

contains

subroutine __lfortran_global_init_coarrays_26_m()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_26_m_x__coarray_handle, __module_coarrays_26_m_x__coarray_data)
        call c_f_pointer(__module_coarrays_26_m_x__coarray_data, x)
    end if
end subroutine __lfortran_global_init_coarrays_26_m

end module coarrays_26_m

module coarrays_26_m2
implicit none
integer(4), pointer :: x

contains

subroutine __lfortran_global_init_coarrays_26_m2()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_26_m2_x__coarray_handle, __module_coarrays_26_m2_x__coarray_data)
        call c_f_pointer(__module_coarrays_26_m2_x__coarray_data, x)
    end if
end subroutine __lfortran_global_init_coarrays_26_m2

end module coarrays_26_m2

module coarrays_26_m3
implicit none
integer(4), pointer :: x

contains

subroutine __lfortran_global_init_coarrays_26_m3()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_26_m3_x__coarray_handle, __module_coarrays_26_m3_x__coarray_data)
        call c_f_pointer(__module_coarrays_26_m3_x__coarray_data, x)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_handle,&
         __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_data)
        call c_f_pointer(__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_data, __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr)
        __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = 10
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_handle,&
         __module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_data)
        call c_f_pointer(__module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_data, __module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_ptr)
        __module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_ptr = 20
    end if
end subroutine __lfortran_global_init_coarrays_26_m3

subroutine coarrays_26_mod_sub()
    __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_&
        ptr + 1
    call coarrays_26_mod_inner()
    if (__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr /= 11) then
        error stop
    end if
    call coarrays_26_mod_host()
    if (__module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr /= 12) then
        error stop
    end if
    contains
    subroutine coarrays_26_mod_host()
        __module_coarrays_26_m3_coarrays_26_mod_sub_x__coarray_ptr = __module_coarrays_26_m3_coarrays_26_mod_sub_x__coar&
        ray_ptr + 1
    end subroutine coarrays_26_mod_host

    subroutine coarrays_26_mod_inner()
        __module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_ptr = __module_coarrays_26_m3_coarr&
        ays_26_mod_sub_coarrays_26_mod_inner_x__coarray_ptr + 1
        if (__module_coarrays_26_m3_coarrays_26_mod_sub_coarrays_26_mod_inner_x__coarray_ptr /= 21) then
            error stop
        end if
    end subroutine coarrays_26_mod_inner

end subroutine coarrays_26_mod_sub

end module coarrays_26_m3

type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor

type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type

type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarrays_26
use coarrays_26_m3, only: coarrays_26_mod_sub
use coarrays_26_m, only: module_x => x
use coarrays_26_m2, only: module_x2 => x
use coarrays_26_m3, only: module_x3 => x
implicit none
integer(4) :: stat
integer(4), pointer :: x
integer(4), save :: x__coarray_ptr = 7
call __lfortran_global_init_coarrays_26()
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
module_x = lcompilers_prif_this_image()
module_x2 = lcompilers_prif_this_image() + 1
module_x3 = lcompilers_prif_this_image() + 2
x = lcompilers_prif_this_image()*10
call coarrays_26_sub()
call coarrays_26_mod_sub()
call coarrays_26_prog_inner()
call __module_prif_prif_sync_all()
if (x /= lcompilers_prif_this_image()*10) then
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
call __module_prif_prif_stop(.false.)

contains

subroutine __lfortran_global_init_coarrays_26()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __lfortran_global_init_coarrays_26_m()
        call __lfortran_global_init_coarrays_26_m2()
        call __lfortran_global_init_coarrays_26_m3()
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle,&
         x__coarray_data)
        call c_f_pointer(x__coarray_data, x)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle1,&
         x__coarray_data1)
        call c_f_pointer(x__coarray_data1, x__coarray_ptr1)
        x__coarray_ptr1 = 40
    end if
end subroutine __lfortran_global_init_coarrays_26

subroutine coarrays_26_prog_inner()
    x__coarray_ptr1 = x__coarray_ptr1 + 1
    if (x__coarray_ptr1 /= 41) then
        error stop
    end if
end subroutine coarrays_26_prog_inner

subroutine __lfortran_coarray_init_coarrays_26_inner_coarrays_26_sub_coarrays_26_sub2()
    integer(4) :: stat
    call __module_prif_prif_init(stat)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle2, x__coarray_data2)
    call c_f_pointer(x__coarray_data2, x__coarray_ptr2)
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle3, x__coarray_data3)
    call c_f_pointer(x__coarray_data3, x__coarray_ptr3)
    x__coarray_ptr3 = 30
    call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), x__coarray_handle4, x__coarray_data4)
    call c_f_pointer(x__coarray_data4, x__coarray_ptr)
end subroutine __lfortran_coarray_init_coarrays_26_inner_coarrays_26_sub_coarrays_26_sub2

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
    subroutine __module_prif_prif_this_image_no_coarray(team, this_image)
        type(__module_prif_prif_team_type), intent(in), optional :: team
        integer(4), intent(out) :: this_image
    end subroutine __module_prif_prif_this_image_no_coarray
end interface

subroutine coarrays_26_sub()
    integer(4) :: x__coarray_data
    integer(4) :: x__coarray_handle
    integer(4) :: x__coarray_ptr
    x__coarray_ptr = -1
    x__coarray_handle = -2
    x__coarray_data = -3
    x__coarray_ptr2 = lcompilers_prif_this_image() + 100
    call coarrays_26_sub2()
    call coarrays_26_inner()
    call coarrays_26_host()
    if (x__coarray_ptr2 /= lcompilers_prif_this_image() + 101) then
        error stop
    end if
    if (x__coarray_ptr /= (-1) .or. x__coarray_handle /= (-2) .or. x__coarray_data /= (-3)) then
        error stop
    end if
    contains
    subroutine coarrays_26_host()
        x__coarray_ptr2 = x__coarray_ptr2 + 1
    end subroutine coarrays_26_host

    subroutine coarrays_26_inner()
        x__coarray_ptr3 = x__coarray_ptr3 + 1
        if (x__coarray_ptr3 /= 31) then
            error stop
        end if
    end subroutine coarrays_26_inner

end subroutine coarrays_26_sub

subroutine coarrays_26_sub2()
    x__coarray_ptr = lcompilers_prif_this_image() + 1000
    if (x__coarray_ptr /= lcompilers_prif_this_image() + 1000) then
        error stop
    end if
end subroutine coarrays_26_sub2

integer(4) function lcompilers_prif_this_image()
    call __module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program coarrays_26
