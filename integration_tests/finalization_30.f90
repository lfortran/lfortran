! A scalar of an extended type that has no final subroutine of its own is
! finalized at END through its parent component (F2018 7.5.6.2 step 3).
module finalization_30_mod
    implicit none
    character(len=100) :: log = ""
    type base
        integer :: c = 0
    contains
        final :: finish_base
    end type
    type, extends(base) :: child
        integer :: z = 0
    end type
    type, extends(child) :: grandchild
        integer :: w = 0
    end type
    type, extends(base) :: own
    contains
        final :: finish_own
    end type
    type item
        integer :: k = 0
    contains
        final :: finish_item
    end type
    type, extends(base) :: with_item
        type(item) :: it
    end type
    type holder
        type(child) :: inner
    end type
contains
    subroutine add(s)
        character(*), intent(in) :: s
        log = trim(log) // s
    end subroutine
    subroutine finish_base(self)
        type(base), intent(inout) :: self
        character(len=8) :: buf
        write(buf, '(i0)') self%c
        call add("b" // trim(buf) // ",")
    end subroutine
    subroutine finish_own(self)
        type(own), intent(inout) :: self
        call add("o,")
    end subroutine
    subroutine finish_item(self)
        type(item), intent(inout) :: self
        call add("i,")
    end subroutine
    subroutine local_child()
        type(child) :: y
        y%c = 7
    end subroutine
    subroutine local_grandchild()
        type(grandchild) :: y
        y%c = 2
    end subroutine
    subroutine local_own()
        type(own) :: y
        y%c = 3
    end subroutine
    subroutine local_with_item()
        type(with_item) :: y
        y%c = 4
    end subroutine
    subroutine local_holder()
        type(holder) :: y
        y%inner%c = 5
    end subroutine
end module

program finalization_30
    use finalization_30_mod
    implicit none
    call local_child()
    print *, trim(log)
    if (log /= "b7,") error stop 1
    log = ""
    call local_grandchild()
    print *, trim(log)
    if (log /= "b2,") error stop 2
    log = ""
    call local_own()
    print *, trim(log)
    if (log /= "o,b3,") error stop 3
    log = ""
    call local_with_item()
    print *, trim(log)
    if (log /= "i,b4,") error stop 4
    log = ""
    call local_holder()
    print *, trim(log)
    if (log /= "b5,") error stop 5
end program
