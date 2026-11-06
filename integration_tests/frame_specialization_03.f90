! With --fast (#16665), the copy of `mix` takes its dummy procedure `f` with
! the frame of its host. Every recursive instance passes its own internal
! procedure `plus_n`, which captures `n` and `f`, on to `mix`, starting from
! a capturing internal procedure, a module procedure and an external
! procedure, so that the same dummy receives all three kinds. Without the
! copy, each instance would hold a thunk of `plus_n`, more than the default
! --internal-procedure-thunks.
integer function external_double(x)
implicit none
integer, intent(in) :: x
external_double = 2*x
end function

module frame_specialization_03_m
implicit none
interface
    integer function external_double(x)
        integer, intent(in) :: x
    end function
end interface
contains
    recursive integer function mix(n, f) result(r)
        integer, intent(in) :: n
        interface
            integer function f(x)
                integer, intent(in) :: x
            end function
        end interface
        if (n == 0) then
            r = f(1)
        else
            r = f(n) + mix(n - 1, plus_n)
        end if
    contains
        integer function plus_n(x)
            integer, intent(in) :: x
            plus_n = f(x) + n
        end function
    end function

    integer function module_square(x)
        integer, intent(in) :: x
        module_square = x*x
    end function

    integer function host(c, depth) result(r)
        integer, intent(in) :: c, depth
        r = mix(depth, add_c)
    contains
        integer function add_c(x)
            integer, intent(in) :: x
            add_c = x + c
        end function
    end function
end module

program frame_specialization_03
use frame_specialization_03_m
implicit none
integer :: r1, r2, r3
r1 = host(7, 20)
r2 = mix(20, module_square)
r3 = mix(20, external_double)
print *, r1, r2, r3
if (r1 /= 3228) error stop 1
if (r2 /= 5741) error stop 2
if (r3 /= 3292) error stop 3
end program
