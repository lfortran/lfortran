module recursion_07_mod
implicit none

type :: fin_t
    integer :: a = 0
contains
    final :: fin
end type

type :: wrap_t
    type(fin_t) :: c
end type

type :: base_t
    integer :: a = 0
end type

type, extends(base_t) :: ext_t
contains
    final :: fin_ext
end type

type :: io_t
    integer :: a = 0
contains
    procedure :: write_io
    generic :: write(formatted) => write_io
end type

type :: plain_t
    integer :: a = 0
end type

interface write(formatted)
    module procedure write_plain
end interface

contains

recursive subroutine fin(x)
    type(fin_t), intent(inout) :: x
    if (x%a > 1) call fin_block(x%a - 1)
    if (x%a < -1) call fin_component(-x%a - 1)
end subroutine fin

recursive subroutine fin_ext(x)
    type(ext_t), intent(inout) :: x
    if (x%a > 1) call fin_class(x%a - 1)
    if (x%a < -1) call fin_pointer(-x%a - 1)
end subroutine fin_ext

recursive subroutine write_io(dtv, unit, iotype, v_list, iostat, iomsg)
    class(io_t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    iostat = 0
    if (dtv%a > 1) call dtio_bound(dtv%a - 1)
end subroutine write_io

recursive subroutine write_plain(dtv, unit, iotype, v_list, iostat, iomsg)
    class(plain_t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    iostat = 0
    if (dtv%a > 1) call dtio_interface(dtv%a - 1)
end subroutine write_plain

! The finalizer of a local of a block in `inner` enters the host again
recursive subroutine fin_block(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        before = mine
        block
            type(fin_t) :: obj
            obj%a = depth
        end block
        if (mine /= before) error stop 1
    end subroutine inner
end subroutine fin_block

! The finalizer of a component of a local of `inner` enters the host again
recursive subroutine fin_component(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        before = mine
        block
            type(wrap_t) :: w
            w%c%a = -depth
        end block
        if (mine /= before) error stop 2
    end subroutine inner
end subroutine fin_component

! The finalizer of the dynamic type of a polymorphic local of `inner`
! enters the host again
recursive subroutine fin_class(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        class(base_t), allocatable :: obj
        before = mine
        allocate(ext_t :: obj)
        obj%a = depth
        deallocate(obj)
        if (mine /= before) error stop 3
    end subroutine inner
end subroutine fin_class

! The finalizer of the dynamic type of a polymorphic pointer deallocated
! in `inner` enters the host again
recursive subroutine fin_pointer(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        class(base_t), pointer :: obj
        before = mine
        allocate(ext_t :: obj)
        obj%a = -depth
        deallocate(obj)
        if (mine /= before) error stop 4
    end subroutine inner
end subroutine fin_pointer

! The defined output bound to the type of a host variable enters the host again
recursive subroutine dtio_bound(depth)
    integer, intent(in) :: depth
    integer :: mine
    type(io_t) :: q
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        before = mine
        q%a = depth
        write(*, '(dt)', advance='no') q
        if (mine /= before) error stop 5
    end subroutine inner
end subroutine dtio_bound

! The defined output of a generic interface enters the host again
recursive subroutine dtio_interface(depth)
    integer, intent(in) :: depth
    integer :: mine
    type(plain_t) :: q
    mine = depth
    call inner()
contains
    subroutine inner()
        integer :: before
        before = mine
        q%a = depth
        write(*, '(dt)', advance='no') q
        if (mine /= before) error stop 6
    end subroutine inner
end subroutine dtio_interface

end module recursion_07_mod

program recursion_07
use recursion_07_mod
implicit none
call fin_block(3)
call fin_component(3)
call fin_class(3)
call fin_pointer(3)
call dtio_bound(3)
call dtio_interface(3)
print *, "ok"
end program recursion_07
