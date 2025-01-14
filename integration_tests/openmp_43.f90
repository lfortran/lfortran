program parallel_example  
  use omp_lib  
  implicit none  

  integer :: i  
  real :: result(10)  

  ! Initialize the result array  
  do i = 1, 10  
     result(i) = 0.0  
  end do  

  ! Parallel region without a DO loop  
  !$omp parallel private(i)  
  i = omp_get_thread_num() + 1  ! Get the thread number (0-based)  
  if (i <= 10) then  
     result(i) = i * 2.0          ! Perform some computation  
  end if  
  !$omp end parallel  

  ! Output the result  
  print *, "Result array:"  
  do i = 1, 10  
     print *, result(i)  
  end do  

end program parallel_example