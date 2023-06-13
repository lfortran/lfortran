module derived_types_23_vtk90m
contains
	subroutine vtk90()
		implicit none

		character(len=6) :: description
		description = "   abc"
		print *, adjustl(description)
	end subroutine vtk90
end module

program derived_types_23
    use derived_types_23_vtk90m
    call vtk90()
end program
