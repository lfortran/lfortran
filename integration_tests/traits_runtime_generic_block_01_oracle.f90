module traits_runtime_generic_block_01_oracle_helpers
    implicit none
contains
    integer function increment(n)
        integer, intent(in) :: n
        increment = n + 1
    end function
end module

module traits_runtime_generic_block_01_oracle_m
    implicit none
    type :: Cell
        integer :: n
    contains
        final :: finish_cell
    end type
    type :: Token
        integer :: n
    contains
        final :: finish_token
    end type
    integer :: bias = 5, finals = 0, total = 0, argument_finals = 0
contains
    integer function cell_value(self)
        type(Cell), intent(in) :: self
        cell_value = self%n
    end function
    subroutine finish_cell(self)
        type(Cell), intent(inout) :: self
        argument_finals = argument_finals + 1
        self%n = -999
    end subroutine
    subroutine finish_token(self)
        type(Token), intent(inout) :: self
        finals = finals + 1
        total = total + self%n
    end subroutine
    function apply(arg, mode) result(r)
        type(Cell), intent(in) :: arg
        integer, intent(in) :: mode
        integer :: r
        r = cell_value(arg)
        outer: block
            use traits_runtime_generic_block_01_oracle_helpers, only: bump => increment
            integer :: local
            type(Token) :: outer_token
            outer_token%n = 10 + mode
            local = bump(r)
            r = local
            block
                integer :: local
                type(Token) :: inner_token
                inner_token%n = 20 + mode
                local = 7
                r = r + local
                if (mode == 1) exit outer
                if (mode == 2) return
                block
                    integer :: nested
                    nested = local + cell_value(arg)
                    r = r + nested
                end block
            end block
            r = r + local
        end block outer
        r = r + bias
    end function
end module

program traits_runtime_generic_block_01_oracle
    use traits_runtime_generic_block_01_oracle_m
    implicit none
    type(Cell) :: object
    integer :: r
    object%n = 31
    r = apply(object, 0)
    if (r /= 114 .or. finals /= 2 .or. total /= 30) error stop 1
    r = apply(object, 1)
    if (r /= 44 .or. finals /= 4 .or. total /= 62) error stop 2
    r = apply(object, 2)
    if (r /= 39 .or. finals /= 6 .or. total /= 96) error stop 3
    bias = 7
    r = apply(object, 0)
    if (r /= 116 .or. finals /= 8 .or. total /= 126) error stop 4
    if (argument_finals /= 0 .or. object%n /= 31) error stop 5
end program
