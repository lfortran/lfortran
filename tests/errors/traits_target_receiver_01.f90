! The receiver of each implementation of a message whose receiver has the
! TARGET attribute must also have it.
module traits_target_receiver_01_m
    implicit none
    abstract interface :: ISomeTrait
        subroutine method(self)
            type(itself), pass, target, intent(in) :: self
        end subroutine method
    end interface

    type, implements(ISomeTrait) :: MyType
    contains
        procedure :: method => my_method
    end type MyType
contains
    subroutine my_method(self)
        class(MyType), intent(in) :: self
    end subroutine my_method
end module traits_target_receiver_01_m
