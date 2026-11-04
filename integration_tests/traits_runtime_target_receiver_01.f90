! A trait message whose receiver has the TARGET attribute, as the reported
! `type(itself), pass, target, intent(in) :: self` with an
! `optional, target` array argument does: an implementation whose receiver
! is also TARGET points at a component of its receiver and at the array,
! called statically and through a runtime view, with and without the array.
module traits_runtime_target_receiver_01_m
    implicit none
    private
    public :: ISomeTrait, MyType

    abstract interface :: ISomeTrait
        subroutine method(self, arr, s)
            type(itself), pass, target, intent(in) :: self
            real(8), optional, target, intent(in) :: arr(:,:)
            real(8), intent(out) :: s
        end subroutine method
    end interface

    type, implements(ISomeTrait) :: MyType
        real(8) :: w = 2
    contains
        procedure :: method => my_method
    end type MyType

contains

    subroutine my_method(self, arr, s)
        class(MyType), target, intent(in) :: self
        real(8), optional, target, intent(in) :: arr(:,:)
        real(8), intent(out) :: s
        real(8), pointer :: p, q(:,:)
        p => self%w
        s = p
        if (present(arr)) then
            q => arr
            s = s * sum(q)
        end if
    end subroutine my_method
end module traits_runtime_target_receiver_01_m

program traits_runtime_target_receiver_01
    use traits_runtime_target_receiver_01_m
    implicit none
    type(MyType), target :: m
    class(ISomeTrait), allocatable :: v
    real(8), target :: a(2, 2)
    real(8) :: s

    a = 1
    a(2, 2) = 3
    call m%method(a, s)
    if (abs(s - 12) > 1e-12_8) error stop 1
    call m%method(s=s)
    if (abs(s - 2) > 1e-12_8) error stop 2
    allocate(v, source=MyType(w=3))
    call v%method(a, s)
    if (abs(s - 18) > 1e-12_8) error stop 3
    call v%method(s=s)
    if (abs(s - 3) > 1e-12_8) error stop 4
    print *, s
end program traits_runtime_target_receiver_01
