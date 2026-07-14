program namelist_derived_allocatable_array
    implicit none

    type :: pair
        integer :: a
        integer :: b
    end type pair

    type(pair), allocatable :: items(:)

    namelist /settings/ items

    read(*, nml=settings)
end program namelist_derived_allocatable_array
