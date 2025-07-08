module fortuno_basetypes
    implicit none
  
    type, abstract :: test_base
      character(:), allocatable :: name
    end type test_base
  
  
    type :: test_ptr_item
        class(test_base), pointer :: item
    end type test_ptr_item
  
  
    type :: test_list
      private
      type(test_ptr_item), pointer :: storage_(:) => null()                       
      integer :: nitems = 0
    contains
      procedure :: free => test_list_free
    end type
  
  
  contains
  
    subroutine test_list_free(this)
      class(test_list), intent(inout) :: this                                     
  
      type(test_ptr_item), target :: d(1)
      this%storage_ => d

      select type (item => this%storage_(1)%item)                                 
      class default
      end select
  
    end subroutine test_list_free
  
  end module fortuno_basetypes
program fortuno_01
    use fortuno_basetypes
    implicit none

    type(test_list) :: my_list
    call my_list%free()

    print *, "Test list has been freed successfully."
end program
