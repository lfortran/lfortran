module stdlib_optval_01
  implicit none
  public :: optval

  interface optval
    module procedure optval_character
  end interface optval

contains
  pure function optval_character(x, default) result(y)
    character(len=*), intent(in), optional :: x
    character(len=*), intent(in) :: default
    character(len=:), allocatable :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_character

end module stdlib_optval_01

module stdlib_bitsets_mod_01
    use stdlib_optval_01, only : optval

    implicit none

    public ::         &
        bitset_type,  &
        bitset_64

    type, abstract :: bitset_type
        integer(4) :: num_bits
    contains
    end type bitset_type

    type, extends(bitset_type) :: bitset_64
        integer(8), private :: block = 0
    contains
        procedure, pass(self)  :: read_bitset_unit => read_bitset_unit_64
    end type bitset_64

contains

    subroutine read_bitset_unit_64(self, unit, advance)
        class(bitset_64), intent(out)      :: self
        integer, intent(in)                :: unit
        character(*), intent(in), optional :: advance
        character(len=1)              :: char
        character(len=:), allocatable :: adv

        adv = optval(advance, 'YES')
        if (adv /= 'YES' .and. adv /= 'NO') error stop
        self%num_bits = 0
    end subroutine read_bitset_unit_64
end module stdlib_bitsets_mod_01

program stdlib_bitsets_01
    use stdlib_bitsets_mod_01
    implicit none
    type(bitset_64) :: b
    call b%read_bitset_unit(10)
    if (b%num_bits /= 0) error stop
    call b%read_bitset_unit(10, 'NO')
    if (b%num_bits /= 0) error stop
end program
