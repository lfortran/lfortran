program where_12
    integer ::arr(3,3,3)
    integer ::arr2(3,3,3)

    arr = 10
    arr2 = 11
    
    arr(:,2,:) = 0
    where(arr == 0)
        arr = 555
        arr2 = 555
    end where
    
    print *, arr
    if(any(arr(:,2,:) /= 555) .or. any(arr(:,1,:) /= 10) .or. any(arr(:,3,:) /= 10)) error stop

    ! Test whether the other array got affected or not, just to make sure assignments inside if are handled correctly by where pass.
    print *, arr2
    if(any(arr2(:,2,:) /= 555) .or. any(arr2(:,1,:) /= 11) .or. any(arr2(:,3,:) /= 11)) error stop
    
end program where_12