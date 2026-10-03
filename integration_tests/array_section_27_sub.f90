subroutine array_section_27_set(arr)
   real(8), dimension(:) :: arr
   arr(1) = 1.111d0
   arr(2) = 2.222d0
   write(*,*) "set:", arr(1), arr(2)
end subroutine array_section_27_set
