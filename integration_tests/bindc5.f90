module thread_data_module_tasks
  use, intrinsic :: iso_c_binding
  implicit none
  type :: thread_data
    type(c_ptr) :: i_ptr
  end type thread_data
end module thread_data_module_tasks

program bindc5
    use thread_data_module_tasks
    use, intrinsic :: iso_c_binding
    implicit none
    integer, target :: i
    integer, pointer :: ptr_i, ptr_i2
    type(thread_data), pointer :: d,d1
    type(thread_data), target :: threadData, taskData
    type(c_ptr) :: threadPtr, taskPtr

    i=0

    threadData%i_ptr = c_loc(i)
    threadPtr = c_loc(threadData)
    call c_f_pointer(threadPtr, d)
    call c_f_pointer(d%i_ptr, ptr_i)

    ptr_i=2

    taskData%i_ptr = d%i_ptr
    taskPtr = c_loc(taskData)
    call c_f_pointer(taskPtr, d1)
    call c_f_pointer(d1%i_ptr, ptr_i2)

    ptr_i2=3

    print*, i,ptr_i2,ptr_i
end program bindc5