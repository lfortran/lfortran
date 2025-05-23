module history_mod_separate_compilation_10a
contains
subroutine rangehist(xhist)
        real, intent(inout) :: xhist(:, :)
        real :: x(25)
        x = 12.912
        xhist = reshape(x, shape(xhist))
        xhist = 99.124
end subroutine
end module
