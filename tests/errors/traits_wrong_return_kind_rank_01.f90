module traits_wrong_return_kind_rank_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        real :: res
        res = real(self%value)
    end function box_get_value
end module traits_wrong_return_kind_rank_01_m

module traits_wrong_argument_type_01_m
    implicit none
    abstract interface :: IConsume
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IConsume :: Box
        procedure, nopass :: consume => consume_value
    end implements
contains
    subroutine consume_value(value)
        real, intent(in) :: value
        print *, value
    end subroutine
end module

module traits_wrong_argument_rank_01_m
    implicit none
    abstract interface :: IConsume
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IConsume :: Box
        procedure, nopass :: consume => consume_value
    end implements
contains
    subroutine consume_value(value)
        integer, intent(in) :: value(1)
        print *, value
    end subroutine
end module

module traits_wrong_return_kind_01_m
    implicit none
    abstract interface :: IValue
        function get_value() result(value)
            integer :: value
        end function
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    function box_value(self) result(value)
        class(Box), intent(in) :: self
        integer(8) :: value
        value = self%value
    end function
end module

module traits_wrong_return_rank_01_m
    implicit none
    abstract interface :: IValue
        function get_value() result(value)
            integer :: value
        end function
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    function box_value(self) result(value)
        class(Box), intent(in) :: self
        integer :: value(1)
        value = [self%value]
    end function
end module
