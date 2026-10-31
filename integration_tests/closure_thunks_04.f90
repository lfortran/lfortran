! Hand-lowered internal-procedure callbacks (#16400) from concurrent host
! activations: OpenMP threads each run an activation of `host`, whose
! internal procedure `get` escapes through a thunk. Acquiring a thunk slot
! is a critical section (at the top level of its procedure, see #14790); each activation must reach its own variable.
module closure_thunks_04_m
implicit none
abstract interface
    integer function getter()
    end function
end interface

type :: host_frame
    integer, pointer :: captured => null()
    integer :: get_thunk = 0
end type

type :: host_frame_ptr
    type(host_frame), pointer :: p => null()
end type

type(host_frame_ptr), save :: get_slot(4)

contains

integer function call_it(f)
    procedure(getter) :: f
    call_it = f()
end function

subroutine host(n, r)
    integer, intent(in) :: n
    integer, intent(out) :: r
    integer, target :: captured
    integer :: i
    type(host_frame), target :: frame
    procedure(getter), pointer :: p
    frame%captured => captured
    captured = n
    p => get_value(frame)
    r = 0
    do i = 1, 1000
        r = r + call_it(p) - n
    end do
    r = r + call_it(p)
    call get_release(frame)
end subroutine

integer function get(frame)
    type(host_frame), intent(in) :: frame
    get = frame%captured
end function

function get_value(frame) result(p)
    type(host_frame), target, intent(inout) :: frame
    procedure(getter), pointer :: p
    if (frame%get_thunk == 0) call get_acquire(frame)
    if (frame%get_thunk == 0) error stop "too many simultaneously live values of internal procedure 'get'"
    select case (frame%get_thunk)
    case (1)
        p => get_thunk_1
    case (2)
        p => get_thunk_2
    case (3)
        p => get_thunk_3
    case (4)
        p => get_thunk_4
    end select
end function

subroutine get_acquire(frame)
    type(host_frame), target, intent(inout) :: frame
    integer :: i
    !$omp critical (get_pool)
    do i = 1, size(get_slot)
        if (.not. associated(get_slot(i)%p)) exit
    end do
    if (i <= size(get_slot)) then
        get_slot(i)%p => frame
        frame%get_thunk = i
    end if
    !$omp end critical (get_pool)
end subroutine

subroutine get_release(frame)
    type(host_frame), intent(inout) :: frame
    integer :: i
    i = frame%get_thunk
    frame%get_thunk = 0
    !$omp critical (get_pool)
    if (i /= 0) nullify(get_slot(i)%p)
    !$omp end critical (get_pool)
end subroutine

integer function get_thunk_1()
    get_thunk_1 = get(get_slot(1)%p)
end function

integer function get_thunk_2()
    get_thunk_2 = get(get_slot(2)%p)
end function

integer function get_thunk_3()
    get_thunk_3 = get(get_slot(3)%p)
end function

integer function get_thunk_4()
    get_thunk_4 = get(get_slot(4)%p)
end function
end module

program closure_thunks_04
use closure_thunks_04_m
implicit none
integer :: i, r(64)
!$omp parallel do num_threads(4) private(i)
do i = 1, 64
    call host(i, r(i))
end do
!$omp end parallel do
do i = 1, 64
    if (r(i) /= i) error stop 1
end do
print *, "ok"
end program
