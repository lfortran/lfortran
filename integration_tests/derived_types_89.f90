program derived_types_89
    implicit none

    type :: package_t
        character(len=100), allocatable :: modules_provided(:)
    end type package_t

    type(package_t) :: empty_pkg, pkg
    pkg = empty_pkg

    print *, allocated(empty_pkg % modules_provided)
    print *, allocated(pkg % modules_provided)
    if (allocated(empty_pkg % modules_provided)) error stop
    if (allocated(pkg % modules_provided)) error stop
end program