module derived_types_158_m

    implicit none

    public :: matrix
    public :: rcurve

    type :: matrix
        sequence
        real :: array(4,4)
    end type matrix

    interface
        subroutine rcurve_f(geom)
            implicit none

            type :: matrix
                sequence
                real :: array(4,4)
            end type matrix

            type(matrix), intent(in) :: geom
        end subroutine rcurve_f
    end interface

contains

    subroutine rcurve(geom)
        implicit none

        type :: matrix
            sequence
            real :: array(4,4)
        end type matrix

        real, intent(in) :: geom(4,4)

        type(matrix) :: geom_matrix

        geom_matrix%array = geom

        call rcurve_f(geom_matrix)

    end subroutine rcurve

end module derived_types_158_m


! Actual implementation
subroutine rcurve_f(geom)
    implicit none

    type :: matrix
        sequence
        real :: array(4,4)
    end type matrix

    type(matrix), intent(in) :: geom

    integer :: i, j

    do i = 1, 4
        do j = 1, 4
            if (geom%array(i, j) /= real(i + 10 * j)) then
                error stop "rcurve_f received wrong data through the SEQUENCE-associated struct"
            end if
        end do
    end do

end subroutine rcurve_f


program derived_types_158

    use derived_types_158_m

    implicit none

    real :: a(4,4)
    integer :: i, j

    do i = 1, 4
        do j = 1, 4
            a(i, j) = real(i + 10 * j)
        end do
    end do

    call rcurve(a)

end program derived_types_158
