module history_mod_separate_compilation_10a
contains
subroutine rangehist(xhist)
        real, intent(inout) :: xhist(:, :)
        xhist = reshape([xhist(:, 5), xhist(:, 5)], shape(xhist))
        xhist = 99.124
end subroutine
end module
