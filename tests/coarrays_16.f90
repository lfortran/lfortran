subroutine coarrays_16_sub(v)
    integer, save :: x[*] = 0

    integer, intent(out) :: v

    x = x + 1
    v = x
end subroutine coarrays_16_sub

program coarrays_16
    integer :: a1
    integer :: b1
    integer, save :: y[*]

    if (this_image() == 2) then
      call coarrays_16_sub(a1)
      call coarrays_16_sub(b1)
    end if

    sync all

    if (this_image() == 2) then
        write(*,'(*(I4))') a1, b1
        if (a1 /= 1 .or. b1 /= 2) error stop
    end if

end program coarrays_16