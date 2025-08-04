! passing array as function arguments (with unknown size at compile time)
! Testing for both interfaces and functions 
MODULE passing_array_01_mod
    implicit none
    INTERFACE test_01_interface
       MODULE PROCEDURE test_01
    END INTERFACE test_01_interface

    INTERFACE test_02_interface
       MODULE PROCEDURE test_02
    END INTERFACE test_02_interface

    INTERFACE test_03_interface
       MODULE PROCEDURE test_03
    END INTERFACE test_03_interface

 CONTAINS

    pure FUNCTION func() result (x)
        integer ::  x 
        x = 10
    end FUNCTION func

   ! Array dimension is call to function 
   FUNCTION test_01 (len,value) result(res)
        implicit none
        integer  ,intent(in):: len 
        INTEGER, DIMENSION(func()),intent(in) :: value
        integer:: res
        res = size(value)    
   END FUNCTION test_01

   ! Array dimension is Variable
   FUNCTION test_02 (len,value) result(res)
        implicit none
        integer  ,intent(inout):: len
        INTEGER, DIMENSION(len),intent(in) :: value
        integer:: res
        res = size(value)
   END FUNCTION test_02

   ! Array dimension is variable + function call + constant
   FUNCTION test_03 (len,value) result(res)
        implicit none
        integer  ,intent(in):: len 
        INTEGER, DIMENSION(len + func() + 10),intent(in) :: value
        integer:: res
        res = size(value)
   END FUNCTION test_03

    SUBROUTINE test_entry
      implicit none
      INTEGER :: len
      INTEGER, DIMENSION(40) :: arr
      INTEGER :: i
      INTEGER :: ret
      len = 20
      arr = [(i, i = 1, 40)]
      ! All calls would accept the passed array,
      ! because the passed array's length is greater than or equal to the expected length in the function

      ret =  test_01_interface (len,arr)
      print * , ret
      if (ret /= 10) error stop

      ret =  test_01 (len,arr)
      print * , ret
      if (ret /= 10) error stop

      ret =  test_02_interface (len,arr)
      print * , ret
      if (ret /= 20) error stop

      ret =  test_02 (len,arr)
      print * , ret
      if (ret /= 20) error stop

      ret =  test_03_interface (len,arr)
      print * , ret
      if (ret /= 40) error stop

      ret =  test_03 (len,arr)
      print * , ret
      if (ret /= 40) error stop


   END SUBROUTINE test_entry
 END MODULE passing_array_01_mod

 program passing_array_01
    use passing_array_01_mod
    implicit none
    call test_entry
 end program passing_array_01
