module modules_78_mod
    implicit none
    ! The bare PRIVATE statement comes after these declarations. It still
    ! makes every name without an explicit access-spec private (F2018 8.6.1
    ! applies it to the whole specification part), so `hidden_count` must
    ! not be exported while `shown_count` (inline PUBLIC) and `bump`
    ! (PUBLIC statement) must.
    integer, save :: hidden_count = 0
    integer, public, save :: shown_count = 0
    private
    public :: bump
contains
    subroutine bump()
        hidden_count = hidden_count + 1
        shown_count = shown_count + 2
    end subroutine bump
end module modules_78_mod

program modules_78
    use modules_78_mod
    implicit none
    ! Legal only because the module does not export this name.
    integer :: hidden_count
    hidden_count = 10
    call bump()
    call bump()
    if (hidden_count /= 10) error stop
    if (shown_count /= 4) error stop
    print *, "OK"
end program modules_78
