! DEALLOCATE finalizes an allocatable or pointer scalar of an extended type
! through its parent component, and a polymorphic one exactly once, as its
! dynamic type (F2018 7.5.6.2, 7.5.6.3 p2).
module finalization_32_mod
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
end module

program finalization_32
    use finalization_32_mod
    implicit none
    type(child), allocatable :: a
    type(child), pointer :: p
    class(base), allocatable :: x
    class(*), allocatable :: u

    allocate(a)
    a%c = 1
    deallocate(a)
    print *, trim(log)
    if (log /= "b1,") error stop 1

    allocate(p)
    p%c = 2
    deallocate(p)
    print *, trim(log)
    if (log /= "b1,b2,") error stop 2

    allocate(child :: x)
    x%c = 3
    deallocate(x)
    print *, trim(log)
    if (log /= "b1,b2,b3,") error stop 3

    allocate(base :: x)
    x%c = 4
    deallocate(x)
    print *, trim(log)
    if (log /= "b1,b2,b3,b4,") error stop 4

    allocate(child :: u)
    select type (u)
    type is (child)
        u%c = 5
    end select
    deallocate(u)
    print *, trim(log)
    if (log /= "b1,b2,b3,b4,b5,") error stop 5
end program
