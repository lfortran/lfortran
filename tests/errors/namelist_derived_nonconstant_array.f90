subroutine read_items(n)
    implicit none

    integer, intent(in) :: n

    type :: pair
        integer :: a
        integer :: b
    end type pair

    type(pair) :: items(n)

    namelist /settings/ items

    read(*, nml=settings)
end subroutine read_items

program namelist_derived_nonconstant_array
    implicit none

    call read_items(2)

end program namelist_derived_nonconstant_array
