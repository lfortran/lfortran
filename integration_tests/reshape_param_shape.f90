! Test for Issue 4649: reshape called in shape parameter of reshape function 
program main
    implicit none

    complex(8) :: test_reshape_cmplx(2,3),  expected_reshape_cmplx(2,3)
    
    test_reshape_cmplx = reshape([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], reshape(shape(test_reshape_cmplx), [2]))
    expected_reshape_cmplx =  reshape([(1, 2),(3, 4),(5, 6), (7, 8),(9, 10), (11, 12)], shape(expected_reshape_cmplx))


    if (any(test_reshape_cmplx /= expected_reshape_cmplx)) error stop
    print *, test_reshape_cmplx
end program