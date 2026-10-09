module traits_runtime_separate_01_a_m
    use traits_runtime_separate_01_contracts_m, only: IValue
    implicit none
    type :: Payload
        integer :: value
    contains
        procedure :: unrelated => unrelated_a
    end type Payload
    implements IValue :: Payload
        procedure, pass :: value => a_value
        procedure, pass(self) :: affine => a_affine
        procedure, nopass :: tag => a_tag
        procedure, pass(self) :: message => a_message
    end implements Payload
contains
    function a_value(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = self%value
    end function a_value
    function a_affine(left, self, right) result(r)
        integer, intent(in) :: left, right
        class(Payload), intent(in) :: self
        integer :: r
        r = self%value + 10 * left + right
    end function a_affine
    function a_tag(code) result(r)
        integer, intent(in) :: code
        integer :: r
        r = 400 + code
    end function a_tag
    subroutine a_message(left, self, right, result)
        integer, intent(in) :: left, right
        class(Payload), intent(in) :: self
        integer, intent(out) :: result
        result = self%value + 10 * left + right
    end subroutine a_message
    function unrelated_a(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = -self%value
    end function unrelated_a
end module traits_runtime_separate_01_a_m
