program bignumcase
    use iso_fortran_env
    implicit none
  
    integer(int64) :: val
    integer :: neg_count
    val = 98765432101_int64
  
    neg_count = 0
    select case (val)
        case (1:)
            neg_count = neg_count + 1
    end select
  
    print *, neg_count
    IF(neg_count /= 1) error stop
  
  end program