module m_draw

    use iso_c_binding
    implicit none

    public :: matrix
    public :: rcurve

    type, bind(c) :: matrix
        real(c_float) :: array(4,4)
    end type matrix

    interface
        subroutine rcurve_f(geom) bind(c, name='draw_rcurve')
            use iso_c_binding
            implicit none

            type, bind(c) :: matrix
                real(c_float) :: array(4,4)
            end type matrix

            type(matrix), intent(in) :: geom
        end subroutine rcurve_f
    end interface

contains

    subroutine rcurve(geom)

        use iso_c_binding
        implicit none

        type, bind(c) :: matrix
            real(c_float) :: array(4,4)
        end type matrix

        real(c_float), intent(in) :: geom(4,4)

        type(matrix) :: geom_matrix

        geom_matrix%array = geom

        call rcurve_f(geom_matrix)

    end subroutine rcurve

end module m_draw


! Actual implementation
subroutine draw_rcurve(geom) bind(c, name='draw_rcurve')

    use iso_c_binding
    implicit none

    type, bind(c) :: matrix
        real(c_float) :: array(4,4)
    end type matrix

    type(matrix), intent(in) :: geom

end subroutine draw_rcurve


program test

    use iso_c_binding
    use m_draw

    implicit none

    real(c_float) :: a(4,4)

    a = 5.0_c_float

    call rcurve(a)

end program test