module traits_runtime_scalar_01_m
    implicit none
    type :: Argument
        integer :: number
    end type Argument
    abstract interface :: IScalar
        function evaluate(real_arg, complex_arg, logical_arg, text, object) result(r)
            real(8), intent(in) :: real_arg
            complex(8), intent(in) :: complex_arg
            logical, intent(in) :: logical_arg
            character(*), intent(in) :: text
            type(Argument), intent(in) :: object
            integer(8) :: r
        end function evaluate
        subroutine add_to(value)
            integer, intent(inout) :: value
        end subroutine add_to
        function text_length(n, text) result(r)
            integer, intent(in) :: n
            character(n), intent(in) :: text
            integer :: r
        end function text_length
    end interface IScalar
    type :: Payload
        integer :: number
    end type Payload
    implements IScalar :: Payload
        procedure, pass :: evaluate => do_evaluate
        procedure, nopass :: add_to => do_add
        procedure, pass(self) :: text_length => do_text_length
    end implements Payload
contains
    function do_evaluate(self, real_arg, complex_arg, logical_arg, text, object) result(r)
        type(Payload), intent(in) :: self
        real(8), intent(in) :: real_arg
        complex(8), intent(in) :: complex_arg
        logical, intent(in) :: logical_arg
        character(*), intent(in) :: text
        type(Argument), intent(in) :: object
        integer(8) :: r
        if (.not. logical_arg) error stop 1
        if (text /= "witness") error stop 2
        r = self%number + int(real_arg, 8) + int(real(complex_arg), 8) &
            + int(aimag(complex_arg), 8) + len(text) + object%number
    end function do_evaluate
    subroutine do_add(value)
        integer, intent(inout) :: value
        value = value + 7
    end subroutine do_add
    function do_text_length(length, text_value, self) result(r)
        integer, intent(in) :: length
        character(length), intent(in) :: text_value
        type(Payload), intent(in) :: self
        integer :: r
        r = len(text_value) + self%number
    end function do_text_length
    subroutine check(object)
        class(IScalar), intent(in) :: object
        type(Argument) :: arg
        integer :: value
        arg%number = 19
        if (object%evaluate(2.0_8, (3.0_8, 5.0_8), .true., "witness", arg) /= 47_8) error stop 3
        value = 31
        call object%add_to(value)
        if (value /= 38) error stop 4
        if (object%text_length(7, "witness") /= 18) error stop 5
        if (object%text_length(n=3, text="abc") /= 14) error stop 6
    end subroutine check
end module traits_runtime_scalar_01_m

program traits_runtime_scalar_01
    use traits_runtime_scalar_01_m
    implicit none
    type(Payload) :: object
    object%number = 11
    call check(object)
end program traits_runtime_scalar_01
