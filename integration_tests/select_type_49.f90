module select_type_49_mod
    use iso_fortran_env, only: real32
    implicit none
  
    abstract interface
       pure function acc_func(predicted, expected) result(output)
         import real32
         real(real32), dimension(:,:), intent(in) :: predicted, expected
         real(real32), dimension(size(predicted,2)) :: output
       end function acc_func
    end interface
  
    type :: my_type
       procedure(acc_func), nopass, pointer :: get_accuracy => null()
    end type
  contains
    function eval_accuracy(this, output) result(accuracy)
      class(my_type), intent(in) :: this
      class(*), dimension(:,:), intent(in) :: output
      real(real32) :: accuracy
      real(real32) :: pred(2,3)
      pred = 1.0
      accuracy = 0.0
      select type(output)
      type is(real(real32))
         accuracy = sum(this%get_accuracy(pred, output(:,1:3:1)))
      end select
    end function
  end module
  
program select_type_49
    use select_type_49_mod
    implicit none
    print *, "PASS"
end program select_type_49