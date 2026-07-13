module coarrays_06_m
    integer :: x[*]
contains
    subroutine show()
        print *, x[2]
    end subroutine
end module coarrays_06_m

program coarrays_06
    use coarrays_06_m
    call show()
    sync all
end program coarrays_06
