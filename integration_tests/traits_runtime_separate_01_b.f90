module traits_runtime_separate_01_b_m
    use traits_runtime_separate_01_contracts_m, only: IValue
    implicit none
    type :: Payload
        integer :: value
    contains
        procedure :: extra => unrelated_b
    end type Payload
    implements IValue :: Payload
        procedure, nopass :: tag => b_tag
        procedure, pass(self) :: message => b_message
        procedure, pass(self) :: affine => b_affine
        procedure, pass :: value => b_value
    end implements Payload
contains
    function b_value(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = 2 * self%value + 7
    end function b_value
    function b_affine(left, self, right) result(r)
        integer, intent(in) :: left, right
        class(Payload), intent(in) :: self
        integer :: r
        r = 2 * self%value + 7 + 100 * left + right
    end function b_affine
    function b_tag(code) result(r)
        integer, intent(in) :: code
        integer :: r
        r = 900 + code
    end function b_tag
    subroutine b_message(left, self, right, result)
        integer, intent(in) :: left, right
        class(Payload), intent(in) :: self
        integer, intent(out) :: result
        result = 2 * self%value + 7 + 100 * left + right
    end subroutine b_message
    function unrelated_b(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = -2 * self%value
    end function unrelated_b
end module traits_runtime_separate_01_b_m
