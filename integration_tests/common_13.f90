block data
      integer myint, myzero
      integer myarr
      common /myblock/ myint, myzero, /arrblock/ myarr(10)
      data myint/44/
      data myarr/1,2,3,4,5,6,7,8,9,10/
end

program common_13
    implicit none
    integer myint, myzero
    integer myarr
    common /myblock/ myint, myzero, /arrblock/ myarr(10)

    if (myint /= 44) error stop
    if (myzero /= 0) error stop
    if (any(myarr /= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])) error stop
end program
