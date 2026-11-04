! A message that an abstract adopter leaves to its extensions is called
! through a class(Parent) object like any message: a defining receiver must
! be definable.
module traits_abstract_adopter_01_m
    implicit none
    abstract interface :: IDeferred
        subroutine bump(self)
            type(itself), pass, intent(inout) :: self
        end subroutine bump
    end interface

    type, abstract, implements(IDeferred) :: Parent
    end type Parent
contains
    subroutine use_it(x)
        class(Parent), intent(in) :: x
        call x%bump()
    end subroutine use_it
end module traits_abstract_adopter_01_m
