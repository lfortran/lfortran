! Instantiation bodies are copies of complete template bodies, whatever the
! order of the template, the instantiation and the host's template blocks
! (#13451).
module template_contains_order_02_m
implicit none
template tmpl {T}
    deferred type :: T
contains
    subroutine bump(i)
        integer, intent(inout) :: i
        i = i + 10
    end subroutine
end template
contains
template subroutine g{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    instantiate tmpl {t}, only: bump_t => bump
    call bump_t(i)
end subroutine
subroutine s(i)
    integer, intent(inout) :: i
    call g{real}(i, 1.0)
end subroutine
end module

subroutine sub_host(i)
implicit none
integer, intent(inout) :: i
call add2{real}(i, 1.0)
contains
template subroutine add2{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    i = i + 2
end subroutine
end subroutine

integer function fun_host(i) result(r)
implicit none
integer, intent(in) :: i
r = i
call add3{integer}(r, 1)
contains
template subroutine add3{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    i = i + 3
end subroutine
end function

program template_contains_order_02
use template_contains_order_02_m, only: s
implicit none
template tmpl2 {T}
    deferred type :: T
contains
    subroutine bump2(i)
        integer, intent(inout) :: i
        i = i + 100
    end subroutine
end template
interface
    subroutine sub_host(i)
    integer, intent(inout) :: i
    end subroutine
    integer function fun_host(i)
    integer, intent(in) :: i
    end function
end interface
integer :: i
i = 1
call s(i)
if (i /= 11) error stop
call h{real}(i, 1.0)
if (i /= 111) error stop
call outer{real}(i, 1.0)
if (i /= 1) error stop
call sub_host(i)
if (i /= 3) error stop
i = fun_host(i)
if (i /= 6) error stop
print *, i
contains
template subroutine h{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    instantiate tmpl2 {t}, only: bump2_t => bump2
    call bump2_t(i)
end subroutine
template subroutine outer{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    call inner{t}(i, x)
end subroutine
template subroutine inner{t}(i, x)
    deferred type :: t
    integer, intent(inout) :: i
    type(t), intent(in) :: x
    i = 1
end subroutine
end program
