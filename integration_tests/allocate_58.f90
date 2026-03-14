program allocate_58
	implicit none

	type :: IndepVar
		integer :: value
	end type IndepVar

	type :: Parser_t
		type(IndepVar), dimension(:), allocatable :: indepVars
	end type Parser_t

	type(Parser_t) :: parser
	integer :: nIndepVars
	integer :: i

	nIndepVars = 4
	allocate(parser%indepVars(1:nIndepVars))

	if (.not. allocated(parser%indepVars)) error stop 1
	if (lbound(parser%indepVars, 1) /= 1) error stop 2
	if (ubound(parser%indepVars, 1) /= nIndepVars) error stop 3

	do i = 1, nIndepVars
		parser%indepVars(i)%value = i * 10
	end do

	if (parser%indepVars(1)%value /= 10) error stop 4
	if (parser%indepVars(nIndepVars)%value /= 40) error stop 5

	deallocate(parser%indepVars)
	if (allocated(parser%indepVars)) error stop 6
end program allocate_58