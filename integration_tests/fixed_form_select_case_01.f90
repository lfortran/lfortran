      program fixed_form_select_case_01
          integer :: i, r
          r = 0
        do 50 i = 1, 5
           select case ( i )
              case (1)
                 r = r + 1
              case ( 2 : 3 )
                 r = r + 10
              case default
                 r = r + 100
           end select
   50     continue
          if (r /= 221) error stop
          print *, r
      end
