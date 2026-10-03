program print_12
    use iso_fortran_env, only: output_unit
    implicit none

    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
    character(kind=ucs4, len=:), allocatable :: greeting
    character(len=80) :: record
    integer :: u, i
    ! "Hello = Ni Hao = " followed by the UTF-8 bytes of U+4F60 and U+597D
    integer, parameter :: tail(6) = [228, 189, 160, 229, 165, 189]

    if (ucs4 /= 4) error stop

    greeting = ucs4_'Hello = Ni Hao = ' // &
         char(int(z'4F60'), ucs4) // char(int(z'597D'), ucs4)
    if (len(greeting) /= 19) error stop 1

    open(output_unit, encoding='UTF-8')
    print *, greeting

    ! Write the same value to a file and read it back as default characters,
    ! so the bytes that reach the stream can be checked.
    open(newunit=u, file='print_12_data.txt', status='replace', &
         encoding='UTF-8', action='write')
    write(u, '(a)') greeting
    close(u)

    open(newunit=u, file='print_12_data.txt', status='old', action='read')
    read(u, '(a)') record
    close(u, status='delete')

    if (record(1:17) /= 'Hello = Ni Hao = ') error stop 2
    do i = 1, 6
        if (ichar(record(17 + i : 17 + i)) /= tail(i)) error stop 3
    end do
end program print_12
