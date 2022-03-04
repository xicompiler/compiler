let ok_if_true_lazy ~error b = if b then Ok () else Error (error ())
