module func_call_in_decl_04_m1
implicit none
contains

pure integer function n()
    n = 2
end function n

end module func_call_in_decl_04_m1

module func_call_in_decl_04_m2
implicit none
contains

! The result length calls `n`, which is use associated only in `f`.
function f() result(r)
    use func_call_in_decl_04_m1, only: n
    character(len=n()) :: r
    r = "abc"
end function f

end module func_call_in_decl_04_m2

program func_call_in_decl_04
use func_call_in_decl_04_m2, only: f
implicit none
! A local `n` of the caller, unrelated to the `n` in the result length of `f`
integer :: n
character(len=:), allocatable :: s
n = 5
s = f()
print *, len(s), s
if (len(s) /= 2) error stop
if (s /= "ab") error stop
if (len(f()) /= 2) error stop
if (n /= 5) error stop
end program func_call_in_decl_04
