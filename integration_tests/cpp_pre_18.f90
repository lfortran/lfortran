#define ADD3(a, b, c) print *, a, b, c

#define SUM3(a, b, c) ((a) + (b) + (c))

program cpp_pre_18
    implicit none
    integer :: x = 1, y = 2, z = 3
    integer :: s

    ! Macro invocation split across three physical lines using "\<newline>".
    ADD3(x, \
         y, \
         z)

    ! Expression that would trip the Fortran tokenizer if "\" leaks through.
    s = SUM3(x, \
             y, \
             z)
    if (s /= 6) error stop
    print *, s
end program cpp_pre_18