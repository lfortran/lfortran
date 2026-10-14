      program fixed_form_named_block
      integer value, endblockfoo, initialized
      common /block_data_values/ initialized
      value = 1
      endblockfoo = 3
      if (endblockfoo .ne. 3) error stop
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
      data: block
        integer local_data
        local_data = 1
      end block data
      database: block
        integer local_database
        local_database = 2
      end block database
      if (initialized .ne. 7) error stop
      end
      block data fixed_form_block_data
      integer initialized
      common /block_data_values/ initialized
      data initialized /7/
      end block data fixed_form_block_data
