module stdlib_bitsets
    implicit none
    public ::         &
        bitset_type,  &
        bitset_large, &
        bitset_64,    &
        bitset_64_concrete

    type, abstract :: bitset_type
        integer(4) :: num_bits
    contains
        procedure(clear_bit_abstract), deferred, pass(self)   :: clear_bit
        procedure(clear_range_abstract), deferred, pass(self) :: clear_range
        generic :: clear => clear_bit, clear_range
    end type bitset_type

    abstract interface
        elemental subroutine clear_bit_abstract(self, pos)
            import :: bitset_type
            class(bitset_type), intent(inout) :: self
            integer(4), intent(in) :: pos
        end subroutine clear_bit_abstract

        pure subroutine clear_range_abstract(self, start_pos, stop_pos)
            import :: bitset_type
            class(bitset_type), intent(inout) :: self
            integer(4), intent(in) :: start_pos, stop_pos
        end subroutine clear_range_abstract
    end interface

    type, abstract, extends(bitset_type) :: bitset_large
        private
        integer(8), private, allocatable :: blocks(:)
    contains
    end type bitset_large

    type, abstract, extends(bitset_type) :: bitset_64
        private
        integer(8), private :: block = 0
    contains
    end type bitset_64

    type, extends(bitset_64) :: bitset_64_concrete
    contains
        procedure, pass(self) :: clear_bit   => clear_bit_impl
        procedure, pass(self) :: clear_range => clear_range_impl
    end type bitset_64_concrete

contains

    elemental subroutine clear_bit_impl(self, pos)
        class(bitset_64_concrete), intent(inout) :: self
        integer(4), intent(in) :: pos
        ! Dummy implementation
    end subroutine clear_bit_impl

    pure subroutine clear_range_impl(self, start_pos, stop_pos)
        class(bitset_64_concrete), intent(inout) :: self
        integer(4), intent(in) :: start_pos, stop_pos
        ! Dummy implementation
    end subroutine clear_range_impl

end module stdlib_bitsets

program derived_types_50
  use stdlib_bitsets
  implicit none

  type(bitset_64_concrete) :: set0, set1
  set0%num_bits = 64
  set1 = set0

  if (set1%num_bits /= 64) error stop
end program derived_types_50
