program print_12
    use iso_fortran_env, only: output_unit
    implicit none

    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')

    if (ucs4 /= 4) error stop

    open(output_unit, encoding='UTF-8')
    print *, ucs4_'Hello = Ni Hao = ' // &
         char(int(z'4F60'), ucs4) // char(int(z'597D'), ucs4)
end program print_12
