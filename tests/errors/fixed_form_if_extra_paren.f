      subroutine idd_frm(a)
         character a*10
         if (a == sayhello())) error stop

      contains

         function sayhello() result(res)
            character res*5
            res = "hello"
         end function sayhello
      end
