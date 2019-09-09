;;; test that all loads work/compile properly
;;; does double duty of getting coverage of untested modules
(use-modules (schingle content-type)
             (schingle handler)
             (schingle query)
             (schingle route)
             (schingle schingle)
             ;(schingle sinatra)
             (schingle static)
             (schingle util))
