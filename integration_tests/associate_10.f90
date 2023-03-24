program associate_10
    implicit none
    print *, "working ok"
    contains
    subroutine add_sources_from_dir(sources,directory,scope,with_executables,recurse,error)
        integer, intent(inout) :: sources
        character(*), intent(in) :: directory
        integer, intent(in) :: scope
        logical, intent(in), optional :: with_executables
        logical, intent(in), optional :: recurse
        integer, intent(out) :: error
    end subroutine

    subroutine check()
        integer :: i, j, k
        i = 10
        j = 120
        k = 429
        do i=1,3
            associate(s=>i)
                call add_sources_from_dir(i, "lib_dir", k, &
                                error=j)
            end associate
        end do
    end subroutine

end program associate_10
