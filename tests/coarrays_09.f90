subroutine coarrays_09_sub(v)
    integer, save :: x[*] = 0

    integer, intent(out) :: v

    x = x + 1
    v = x
end subroutine coarrays_09_sub

program coarrays_09
    integer :: a1
    integer :: b1

    if (this_image() == 2) then
      call coarrays_09_sub(a1)
      call coarrays_09_sub(b1)
    end if

    sync all

    if (this_image() == 2) then
        write(*,'(*(I4))') a1, b1
        if (a1 /= 1 .or. b1 /= 2) error stop
    end if

end program coarrays_09