! a scalar allocatable passed to an allocatable assumed-rank dummy sees the
! callee's allocate and deallocate through procedure pointers, dummy
! procedures, procedure pointer components and type-bound procedures
module select_rank_53_m
    implicit none
    abstract interface
        subroutine slot_sub(slot, v)
            integer, allocatable, intent(inout) :: slot(..)
            integer, intent(in) :: v
        end subroutine
        integer function slot_fn(slot, v)
            integer, allocatable, intent(inout) :: slot(..)
            integer, intent(in) :: v
        end function
    end interface

    type :: holder
        procedure(slot_sub), pointer, nopass :: p => null()
    end type

    type :: t
    contains
        procedure, nopass :: set_nopass => set
        procedure :: set_pass
        procedure :: set_fn
    end type
contains
    ! allocates and sets an unallocated slot, deallocates an allocated one
    subroutine set(slot, v)
        integer, allocatable, intent(inout) :: slot(..)
        integer, intent(in) :: v
        select rank (slot)
        rank (0)
            if (allocated(slot)) then
                deallocate(slot)
            else
                allocate(slot)
                slot = v
            end if
        rank default
            error stop 99
        end select
    end subroutine

    integer function set_f(slot, v) result(r)
        integer, allocatable, intent(inout) :: slot(..)
        integer, intent(in) :: v
        call set(slot, v)
        r = v
    end function

    subroutine set_pass(self, slot, v)
        class(t), intent(in) :: self
        integer, allocatable, intent(inout) :: slot(..)
        integer, intent(in) :: v
        call set(slot, v)
    end subroutine

    integer function set_fn(self, slot, v) result(r)
        class(t), intent(in) :: self
        integer, allocatable, intent(inout) :: slot(..)
        integer, intent(in) :: v
        r = set_f(slot, v)
    end function

    subroutine call_dummy(cb, k, v)
        procedure(slot_sub) :: cb
        integer, allocatable, intent(inout) :: k
        integer, intent(in) :: v
        call cb(k, v)
    end subroutine

    integer function call_dummy_fn(cb, k, v) result(r)
        procedure(slot_fn) :: cb
        integer, allocatable, intent(inout) :: k
        integer, intent(in) :: v
        r = cb(k, v)
    end function

    subroutine check(k, v, code)
        integer, allocatable, intent(in) :: k
        integer, intent(in) :: v, code
        if (.not. allocated(k)) error stop code
        if (k /= v) error stop code + 1
    end subroutine
end module

program select_rank_53
    use select_rank_53_m
    implicit none
    procedure(slot_sub), pointer :: sp
    procedure(slot_fn), pointer :: fp
    type(holder) :: h
    class(t), allocatable :: o
    integer, allocatable :: k
    integer :: r

    sp => set
    call sp(k, 1)
    call check(k, 1, 10)
    call sp(k, 1)
    if (allocated(k)) error stop 12

    fp => set_f
    r = fp(k, 2)
    if (r /= 2) error stop 20
    call check(k, 2, 21)
    r = fp(k, 2)
    if (allocated(k)) error stop 23

    call call_dummy(set, k, 3)
    call check(k, 3, 30)
    call call_dummy(set, k, 3)
    if (allocated(k)) error stop 32

    r = call_dummy_fn(set_f, k, 4)
    call check(k, 4, 40)
    r = call_dummy_fn(set_f, k, 4)
    if (allocated(k)) error stop 42

    h%p => set
    call h%p(k, 5)
    call check(k, 5, 50)
    call h%p(k, 5)
    if (allocated(k)) error stop 52

    allocate(o)
    call o%set_nopass(k, 6)
    call check(k, 6, 60)
    call o%set_nopass(k, 6)
    if (allocated(k)) error stop 62

    call o%set_pass(k, 7)
    call check(k, 7, 70)
    call o%set_pass(k, 7)
    if (allocated(k)) error stop 72

    r = o%set_fn(k, 8)
    if (r /= 8) error stop 80
    call check(k, 8, 81)
    r = o%set_fn(k, 8)
    if (allocated(k)) error stop 83
    print *, "OK"
end program
