! Intrinsic assignment to a scalar of an extended type that has no final
! subroutine of its own finalizes the variable, and so its parent component
! (F2018 7.5.6.3 p1, 7.5.6.2 step 3).
module finalization_31_mod
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
    subroutine assign_variable()
        type(child) :: y, x
        y%c = 7
        x%c = 8
        x%z = 1
        y = x
        if (log /= "b7,") error stop 1
        if (y%c /= 8 .or. y%z /= 1) error stop 2
        log = ""
    end subroutine
    subroutine assign_constructor()
        type(child) :: y
        y%c = 5
        y = child(6, 2)
        if (log /= "b5,") error stop 3
        if (y%c /= 6 .or. y%z /= 2) error stop 4
    end subroutine
    subroutine assign_grandchild()
        type(grandchild) :: y, x
        y%c = 3
        x%c = 4
        y = x
        if (log /= "b3,") error stop 5
        if (y%c /= 4) error stop 6
        log = ""
    end subroutine
end module

program finalization_31
    use finalization_31_mod
    implicit none
    call assign_variable()
    print *, trim(log)
    if (log /= "b8,b8,") error stop 7
    log = ""
    call assign_constructor()
    log = ""
    call assign_grandchild()
    print *, trim(log)
    if (log /= "b4,b4,") error stop 8
end program
