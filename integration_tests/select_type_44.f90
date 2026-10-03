program select_type_44
    use iso_fortran_env, only: int8, int16
    implicit none

    integer(int8) :: a = 2
    integer(int16) :: b = 3
    call set_single(a, b)

    if (b /= 2) error stop

contains

    subroutine set_single(generic0, generic1)
        class(*), intent(in)  :: generic0
        class(*), intent(out) :: generic1

        call set_generic(generic0, generic1)

    end subroutine set_single


    subroutine set_generic(generic0, gen)
        class(*), intent(in)  :: generic0
        class(*), intent(out) :: gen

        select type(g0 => generic0)

        type is (integer(int8))

            select type(g => gen)
            type is (integer(int8))
                g = g0
            type is (integer(int16))
                g = g0
            end select

        end select

    end subroutine set_generic

end program select_type_44
