module allocate59_string
	implicit none

	type :: string
		character(:), allocatable :: chars
	contains
		procedure :: len => string_len
	end type string

contains

	pure integer function string_len(self)
		class(string), intent(in) :: self
		string_len = len(self%chars)
	end function string_len

end module allocate59_string


module allocate59_mre
	use allocate59_string, only: string
	implicit none

	abstract interface
		pure subroutine upper_x(str, res)
			character(*), intent(in)         :: str
			character(len(str)), intent(out) :: res
		end subroutine upper_x
	end interface

contains

	pure subroutine to_upper_identity(str, res)
		character(*), intent(in)         :: str
		character(len(str)), intent(out) :: res
		res = str
		if (len(res) /= len(str)) error stop "result length mismatch"
	end subroutine to_upper_identity

	subroutine upper_caller(f, a)
		procedure(upper_x)   :: f
		class(*), intent(in) :: a

		select type (a)
		type is (string)
			block
				! MRE trigger: local char length depends on polymorphic component.
				character(a%len()) :: res
				call f(a%chars, res)
				if (res /= a%chars) error stop "upper_caller callback mismatch"
			end block
		end select
	end subroutine upper_caller

end module allocate59_mre

program allocate_59
	use allocate59_mre, only: upper_caller, to_upper_identity
	use allocate59_string, only: string
	implicit none

	type(string) :: s

	s%chars = "abc"
	call upper_caller(to_upper_identity, s)

end program allocate_59

