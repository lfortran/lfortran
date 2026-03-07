submodule(mini_batch_m) mini_batch_s
contains
  module procedure make_batch
    mini_batch%input_output_pairs = input_output_pairs
  end procedure make_batch
end submodule mini_batch_s
