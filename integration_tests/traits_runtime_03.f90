! Runtime traits are an LFortran extension.
module traits_runtime_03_m
    implicit none
    abstract interface :: IOperations
        function affine(left, right) result(r)
            integer, intent(in) :: left, right
            integer :: r
        end function affine
        function tag(code) result(r)
            integer, intent(in) :: code
            integer :: r
        end function tag
    end interface IOperations

    type :: First
        integer :: payload
    end type First
    type :: Second
        integer :: padding, payload
    end type Second
    implements IOperations :: First
        procedure, pass(self) :: affine => first_affine
        procedure, nopass :: tag => first_tag
    end implements First
    implements IOperations :: Second
        procedure, nopass :: tag => second_tag
        procedure, pass(self) :: affine => second_affine
    end implements Second
contains
    function first_affine(left, self, right) result(r)
        integer, intent(in) :: left, right
        class(First), intent(in) :: self
        integer :: r
        r = self%payload + 10 * left + right
    end function first_affine

    function second_affine(left, self, right) result(r)
        integer, intent(in) :: left, right
        class(Second), intent(in) :: self
        integer :: r
        r = self%payload + 100 * left + right
    end function second_affine

    function first_tag(code) result(r)
        integer, intent(in) :: code
        integer :: r
        r = 400 + code
    end function first_tag

    function second_tag(code) result(r)
        integer, intent(in) :: code
        integer :: r
        r = 900 + code
    end function second_tag

    subroutine check(object, affine_expected, tag_expected)
        class(IOperations), intent(in) :: object
        integer, intent(in) :: affine_expected, tag_expected
        if (object%affine(2, 3) /= affine_expected) error stop 301
        if (object%affine(right=3, left=2) /= affine_expected) error stop 302
        if (object%tag(5) /= tag_expected) error stop 303
    end subroutine check
end module traits_runtime_03_m

program traits_runtime_03
    use traits_runtime_03_m
    implicit none
    type(First) :: a
    type(Second) :: b
    integer :: i
    a%payload = 11
    b%padding = -999
    b%payload = 29
    do i = 0, 1
        if (mod(command_argument_count() + i, 2) == 0) then
            call check(a, 34, 405)
        else
            call check(b, 232, 905)
        end if
    end do
end program traits_runtime_03
