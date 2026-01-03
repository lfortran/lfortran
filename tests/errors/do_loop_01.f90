program main
  implicit none
  integer :: i, j

  do i = 1, 10
    do j = 1, 2
      i = j + 1
    end do
    j = i + 1
    print *, i, j
  end do
end program
