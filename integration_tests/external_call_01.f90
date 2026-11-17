module external_call_01_m
    implicit none
    external :: ext_mod_sub
contains
    subroutine run_mod(res)
        integer, intent(out) :: res
        call ext_mod_sub(res)
    end subroutine
end module

subroutine show
print *, 'ok'
end subroutine

subroutine caller_dummy(cb, val)
    implicit none
    external :: cb
    integer, intent(inout) :: val
    call cb(val)
end subroutine

program external_call_01
    use external_call_01_m, only: run_mod
    implicit none
    external :: show, show_later, bump_val
    integer :: val
    call show()
    val = 10
    call show_later(val)
    if (val /= 11) error stop 1
    call caller_dummy(bump_val, val)
    if (val /= 21) error stop 2
    call run_mod(val)
    if (val /= 42) error stop 3
end program

subroutine show_later(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine

subroutine bump_val(x)
    integer, intent(inout) :: x
    x = x + 10
end subroutine

subroutine ext_mod_sub(x)
    integer, intent(out) :: x
    x = 42
end subroutine
