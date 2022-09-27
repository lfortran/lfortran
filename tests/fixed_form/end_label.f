      subroutine f()
      go to 1
    1 end

      integer function h()
      h = 0
      go to 1
    1 end

      subroutine g()
      if (.true.) then
          go to 1
    1 end if
      end
