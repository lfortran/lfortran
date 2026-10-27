! A temporary hoisted out of a WRITE output list in a procedure with ENTRY
! must be computed right before that WRITE, in every copy of the body.
module entry_29_mod
    implicit none
contains
    integer function bump(j)
        integer, intent(inout) :: j
        j = j + 1
        bump = j
    end function

    integer function ident(j)
        integer, intent(in) :: j
        ident = j
    end function

    subroutine s(k, mode)
        integer :: k, mode, j
        character(len=16) :: buf
        j = 5
        if (mode == 1) goto 20
        write(buf, '(i0)') ident(bump(j))
        read(buf, *) k
        return
20      j = 10
        write(buf, '(i0)') ident(bump(j)) + 100
        read(buf, *) k
        return
    entry e(k, mode)
        j = 40
        write(buf, '(i0)') ident(bump(j))
        read(buf, *) k
    end subroutine
end module

program entry_29
    use entry_29_mod
    implicit none
    integer :: k
    call s(k, 0)
    print '(i0)', k
    if (k /= 6) error stop 1
    call s(k, 1)
    print '(i0)', k
    if (k /= 111) error stop 2
    call e(k, 0)
    print '(i0)', k
    if (k /= 41) error stop 3
end program
