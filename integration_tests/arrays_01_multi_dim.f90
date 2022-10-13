program arrays_01_multi_dim
  implicit none

    integer, parameter :: no_of_height = 3
    integer, parameter :: no_of_row = 2
    integer, parameter :: no_of_col = 3

    integer :: arr(no_of_height, no_of_row, no_of_col)

    integer :: height
    integer :: row
    integer :: col

    do height = 1, no_of_height
      do row = 1, no_of_row
        do col = 1, no_of_col
            arr(height, row, col) = height * row * col
        end do
      end do
    end do

    do height = 1, no_of_height
      write(*,*) "height =", height
      do row = 1, no_of_row
        do col = 1, no_of_col
          write(*,*) arr(height, row, col)
        end do
      end do
    end do

end program arrays_01_multi_dim
