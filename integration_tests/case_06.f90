program cond_03
implicit none
    integer :: a = 1, b = 2
    integer, parameter :: ap = 1, bp = 2

    real :: c = 1.0, d = 2.0
    real, parameter :: cp = 1.0, dp = 2.0

    integer :: marks
    integer, parameter :: marks_fixed = 94

    ! local variable declaration
    character, parameter :: grade_fixed = 'B'

    select case (grade_fixed)

        case ('A')
        print*, "Excellent!"

        case ('B')
            if( a == b ) then
                print *, "a == b"
                if( ap == bp ) then
                    print *, "ap == bp"
                else
                    print *, "ap /= bp"
                end if
            else if( ap == cp ) then
                print *, "ap == cp"
                if( cp == dp ) then
                    print *, "cp == dp"
                else
                    print *, "cp /= dp"
                end if
            else if( c == d ) then
                print *, "c == d"
            end if

        case ('C')
            print*, "Well done"

        case ('D')
            print*, "You passed"

        case ('F')
            print*, "Better try again"

        case default
            print*, "Invalid grade"

    end select

    print*, "Your grade is ", grade_fixed

    marks = 94
    select case (marks)

       case ((40 + bp):)
          print *, "Pass!"

       case (:(39 - ap))
          print *, "Failed!"

       case default
          print*, "Invalid marks"

    end select
    print*, "Your marks are ", marks


    select case (marks_fixed)

      case ((40 + bp):)
         print *, "Pass!"

      case (0:(39 - ap))
         print *, "Failed!"

      case default
         print*, "Invalid marks"

   end select
   print*, "Your marks are ", marks

end
