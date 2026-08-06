module pure_side_effects_04_const_mod
    implicit none
    private
    public :: NZ
    integer, parameter :: NZ = 8
end module pure_side_effects_04_const_mod

module pure_side_effects_04_kern_mod
    use pure_side_effects_04_const_mod, only: NZ
    implicit none
    private
    public :: caller
contains
    ! `h` is an explicit-shape dummy sized by a parameter imported from another
    ! module, so the bound is rewritten into a compiler-generated getter call.
    ! That getter must stay callable from a PURE procedure.
    pure subroutine callee(h, out)
        real, intent(in)  :: h(NZ)
        real, intent(out) :: out
        out = h(1) + h(NZ)
    end subroutine callee

    pure subroutine caller(res)
        real, intent(out) :: res
        real :: buf(NZ)
        integer :: i
        do i = 1, NZ
            buf(i) = real(i)
        end do
        call callee(buf, res)
    end subroutine caller
end module pure_side_effects_04_kern_mod

program pure_side_effects_04
    use pure_side_effects_04_kern_mod, only: caller
    implicit none
    real :: res

    call caller(res)
    if (abs(res - 9.0) > 1e-6) error stop
end program pure_side_effects_04
