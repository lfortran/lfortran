PROGRAM example
    integer :: i
    integer ::arr(4)
    arr =  (  i , i = 1, 4 )
    !arr = (/ (i, i = 1, 4) /)
    print *,arr
 END PROGRAM example