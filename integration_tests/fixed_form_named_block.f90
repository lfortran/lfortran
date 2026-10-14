      program fixed_form_named_block
      integer value
      value = 1
      outer: block
        integer local
        local = 4
        inner: block
          value = local
          exit inner
          value = 99
        end block inner
        if (value .ne. 4) error stop
        block
          value = value + 5
        end block
      end block outer
      if (value .ne. 9) error stop
      end
