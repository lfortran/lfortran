program omp_task_mre
  use omp_lib
  implicit none

  integer, parameter :: n = 10
  real :: array(n)
  integer :: i

  ! Initialize the array
  do i = 1, n
    array(i) = real(i)
  end do

  !$OMP PARALLEL SECTIONS SHARED(array)
    !$OMP SECTION
    do i = 1, n
      !$OMP TASK FIRSTPRIVATE(i) SHARED(array)
        array(i) = array(i) * real(i)
        print *, "Task: i = ", i, ", computed by thread ", omp_get_thread_num()
      !$OMP END TASK
    end do
    !$OMP SECTION
    print*, "All tasks submitted. Waiting for completion."
  !$OMP END PARALLEL SECTIONS

  ! Print the updated array
  print *, "Updated array:"
  do i = 1, n
    print *, "array(", i, ") = ", array(i)
  end do

end program omp_task_mre