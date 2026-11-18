subroutine separate_compilation_77_ext(x)
    use separate_compilation_77a, only: twice
    implicit none
    integer, intent(out) :: x
    x = twice(21)
end subroutine

integer function separate_compilation_77_add(a, b)
    implicit none
    integer, intent(in) :: a, b
    separate_compilation_77_add = a + b
end function
