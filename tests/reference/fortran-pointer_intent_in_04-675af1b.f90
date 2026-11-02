module pointer_intent_in_04_mod
implicit none

contains

subroutine count_present(p, q, r, n)
    integer(4), intent(out) :: n
    integer(4), dimension(:), pointer, intent(in), optional :: p
    integer(4), dimension(:), pointer, intent(in), optional :: q
    integer(4), dimension(:), pointer, intent(in), optional :: r
    n = 0
    if (Present(p)) then
        n = n + size(p)
    end if
    if (Present(q)) then
        n = n + 10*size(q)
    end if
    if (Present(r)) then
        n = n + 100*size(r)
    end if
end subroutine count_present

subroutine forward(x, y, z, n)
    integer(4), dimension(:), pointer :: __libasr_created__subroutine_call_
    integer(4), dimension(:), pointer :: __libasr_created__subroutine_call_1
    integer(4), dimension(:), pointer :: __libasr_created__subroutine_call_2
    integer(4), intent(out) :: n
    integer(4), dimension(:), intent(in), optional, target :: x
    integer(4), dimension(:), intent(in), optional, target :: y
    integer(4), dimension(:), intent(in), optional, target :: z
    if (Present(x)) then
        __libasr_created__subroutine_call_ => x
        if (Present(y)) then
            __libasr_created__subroutine_call_1 => y
            if (Present(z)) then
                __libasr_created__subroutine_call_2 => z
                call count_present(__libasr_created__subroutine_call_, __libasr_created__subroutine_call_1,&
         __libasr_created__subroutine_call_2, n)
            else
                call count_present(__libasr_created__subroutine_call_, __libasr_created__subroutine_call_1, n)
            end if
        else
            if (Present(z)) then
                __libasr_created__subroutine_call_2 => z
                call count_present(__libasr_created__subroutine_call_, __libasr_created__subroutine_call_2, n)
            else
                call count_present(__libasr_created__subroutine_call_, n)
            end if
        end if
    else
        if (Present(y)) then
            __libasr_created__subroutine_call_1 => y
            if (Present(z)) then
                __libasr_created__subroutine_call_2 => z
                call count_present(__libasr_created__subroutine_call_1, __libasr_created__subroutine_call_2, n)
            else
                call count_present(__libasr_created__subroutine_call_1, n)
            end if
        else
            if (Present(z)) then
                __libasr_created__subroutine_call_2 => z
                call count_present(__libasr_created__subroutine_call_2, n)
            else
                call count_present(n)
            end if
        end if
    end if
end subroutine forward

end module pointer_intent_in_04_mod

program pointer_intent_in_04
use pointer_intent_in_04_mod, only: count_present
use pointer_intent_in_04_mod, only: forward
implicit none
integer(4), dimension(1), target :: a
integer(4), dimension(2), target :: b
integer(4), dimension(3), target :: c
integer(4) :: n
a = 1
b = 2
c = 3
call forward(n)
if (n /= 0) then
    error stop
end if
call forward(a, n)
if (n /= 1) then
    error stop
end if
call forward(b, n)
if (n /= 20) then
    error stop
end if
call forward(c, n)
if (n /= 300) then
    error stop
end if
call forward(a, b, n)
if (n /= 21) then
    error stop
end if
call forward(a, c, n)
if (n /= 301) then
    error stop
end if
call forward(b, c, n)
if (n /= 320) then
    error stop
end if
call forward(a, b, c, n)
if (n /= 321) then
    error stop
end if
print *, n
end program pointer_intent_in_04
