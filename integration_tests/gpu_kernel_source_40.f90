program descriptor_inquiry_entry
    implicit none
    integer :: out(4), i, n
    n = 4
    do concurrent (i=1:4)
        out(i) = f(n)
    end do
    if (any(out /= 14)) error stop 1
    if (f(3) /= 12) error stop 2
    call entry_control(out)
contains
    pure integer function f(m) result(r)
        integer, intent(in) :: m
        integer :: head(4)
        integer, pointer :: trigger(:)
        head = 2
        associate (k => head(:m-1))
            block
                integer :: tail(size(k))
                integer :: bounds(ubound(k, 1)-lbound(k, 1)+1)
                tail = 3
                bounds = 4
                r = sum(head) + size(tail) + size(bounds)
            end block
        end associate
    end function

    subroutine entry_control(a)
        integer, intent(in) :: a(:)
        integer :: local(4)
        integer :: b(size(local)), c(ubound(a, 1)-lbound(a, 1)+1)
        integer :: d(sum(shape(a)))
        character(len=size(a)) :: text
        integer :: e(len(text))
        if (size(b) /= 4 .or. size(c) /= 4) error stop 3
        if (size(d) /= 4 .or. size(e) /= 4) error stop 4
    end subroutine
end program
