module coarrays_50_m
implicit none

contains

subroutine __lfortran_global_init_coarrays_50_m()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_50_m_mod_bump_mc__coarray_handle, __module_coarrays_50_m_mod_bump_mc__coarray_data)
        call c_f_pointer(__module_coarrays_50_m_mod_bump_mc__coarray_data, __module_coarrays_50_m_mod_bump_mc__coarray_ptr)
        __module_coarrays_50_m_mod_bump_mc__coarray_ptr = 100
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(),&
         __module_coarrays_50_m_mod_remote_mr__coarray_handle, __module_coarrays_50_m_mod_remote_mr__coarray_data)
        call c_f_pointer(__module_coarrays_50_m_mod_remote_mr__coarray_data, __module_coarrays_50_m_mod_remote_mr__coarray_ptr)
        __module_coarrays_50_m_mod_remote_mr__coarray_ptr = 500
    end if
end subroutine __lfortran_global_init_coarrays_50_m

subroutine mod_bump(v)
    integer(4), intent(out) :: v
    __module_coarrays_50_m_mod_bump_mc__coarray_ptr = __module_coarrays_50_m_mod_bump_mc__coarray_ptr + 1
    v = __module_coarrays_50_m_mod_bump_mc__coarray_ptr
end subroutine mod_bump

subroutine mod_remote(v)
    integer(4), intent(out) :: v
    __module_coarrays_50_m_mod_remote_mr__coarray_ptr = __module_coarrays_50_m_mod_remote_mr__coarray_ptr + lcompilers_p&
        rif_this_image()
    call __module_prif_prif_sync_all()
    v = lcompilers_prif_get_integer(4)(__module_coarrays_50_m_mod_remote_mr__coarray_handle, [int(1, kind=8)], int(0,&
         kind=8))
end subroutine mod_remote

end module coarrays_50_m

type :: __module_prif_prif_dummy_team_descriptor
end type __module_prif_prif_dummy_team_descriptor

type :: __module_prif_prif_team_type
    type(__module_prif_prif_dummy_team_descriptor), pointer :: info
end type __module_prif_prif_team_type

type :: prif_coarray_handle
    type(c_ptr) :: info
end type prif_coarray_handle

program coarrays_50
use coarrays_50_m, only: mod_bump
use coarrays_50_m, only: mod_remote
implicit none
integer(4) :: a
integer(4) :: b
integer(4) :: stat
call __lfortran_global_init_coarrays_50()
call __module_prif_prif_init(stat)
call __module_prif_prif_sync_all()
if (lcompilers_prif_this_image() /= 2) then
    call mod_bump(a)
    if (a /= 101) then
        error stop
    end if
    call mod_bump(b)
    if (b /= 102) then
        error stop
    end if
    call int_bump(a)
    if (a /= 201) then
        error stop
    end if
    call int_bump(b)
    if (b /= 202) then
        error stop
    end if
end if
call mod_remote(a)
if (a /= 501) then
    error stop
end if
call int_remote(b)
if (b /= 601) then
    error stop
end if
if (lcompilers_prif_this_image() == 1) then
    print *, "ok"
end if
call __module_prif_prif_stop(.false.)

contains

subroutine __lfortran_global_init_coarrays_50()
    logical(4), save :: __lfortran_global_init_done = .false.
    integer(4) :: stat
    if (.not. __lfortran_global_init_done) then
        __lfortran_global_init_done = .true.
        call __lfortran_global_init_coarrays_50_m()
        call __module_prif_prif_init(stat)
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), ic__coarray_handle,&
         ic__coarray_data)
        call c_f_pointer(ic__coarray_data, ic__coarray_ptr)
        ic__coarray_ptr = 200
        call __module_prif_prif_allocate_coarray([1_8], [integer(8) :: ], 4_8, null(), ir__coarray_handle,&
         ir__coarray_data)
        call c_f_pointer(ir__coarray_data, ir__coarray_ptr)
        ir__coarray_ptr = 600
    end if
end subroutine __lfortran_global_init_coarrays_50

subroutine int_bump(v)
    integer(4), intent(out) :: v
    ic__coarray_ptr = ic__coarray_ptr + 1
    v = ic__coarray_ptr
end subroutine int_bump

subroutine int_remote(v)
    integer(4), intent(out) :: v
    ir__coarray_ptr = ir__coarray_ptr + lcompilers_prif_this_image()
    call __module_prif_prif_sync_all()
    v = lcompilers_prif_get_integer(4)(ir__coarray_handle, [int(1, kind=8)], int(0, kind=8))
end subroutine int_remote

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
    subroutine __module_prif_prif_get(image_num, coarray_handle, offset, current_image_buffer, size_in_bytes,&
        &
         stat, errmsg, errmsg_alloc)
        type(prif_coarray_handle), intent(in) :: coarray_handle
        type(c_ptr), intent(in) :: current_image_buffer
        character(len=*, kind=1), intent(inout), optional :: errmsg
        character(len=:, kind=1), allocatable, intent(inout), optional :: errmsg_alloc
        integer(4), intent(in) :: image_num
        integer(8), intent(in) :: offset
        integer(8), intent(in) :: size_in_bytes
        integer(4), intent(out), optional :: stat
    end subroutine __module_prif_prif_get
end interface

interface
    subroutine __module_prif_prif_init(stat)
        integer(4), intent(out) :: stat
    end subroutine __module_prif_prif_init
end interface

interface
    subroutine __module_prif_prif_initial_team_index(coarray_handle, sub, initial_team_index, stat)
        type(prif_coarray_handle), intent(in) :: coarray_handle
        integer(4), intent(out) :: initial_team_index
        integer(4), intent(out), optional :: stat
        integer(8), dimension(:), intent(in) :: sub
    end subroutine __module_prif_prif_initial_team_index
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

integer(4) function lcompilers_prif_get_integer(4)(coarray_handle, sub, offset) result(result)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(4) :: image_num
    integer(8), intent(in), value :: offset
    integer(8), dimension(:), intent(in), value :: sub
    call __module_prif_prif_initial_team_index(coarray_handle, sub, image_num)
    call __module_prif_prif_get(image_num, coarray_handle, offset, c_loc(result), 4_8)
end function lcompilers_prif_get_integer(4)

integer(4) function lcompilers_prif_this_image()
    call __module_prif_prif_this_image_no_coarray(lcompilers_prif_this_image)
end function lcompilers_prif_this_image

interface
    subroutine prif_coarray_cleanup_interface(handle) bind(c)
        type(prif_coarray_handle), intent(in), value :: handle
    end subroutine prif_coarray_cleanup_interface
end interface

end program coarrays_50
