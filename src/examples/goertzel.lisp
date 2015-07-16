(RProg
 ;; Top-level definitions
 ((RDefn sample-rate (TArray (IShape ()) TFloat)
         (RExpr (Arr () ((RElt (Float 8000.))))))
  (RDefn tau (TArray (IShape ()) TFloat)
         (RExpr
          (Arr ()
               ((RElt
                 (Float
                  6.283185307179586))))))
  (RDefn
   sinusoid*
   (TDProd ((wl SNat))
           (TAll (wt)
                 (TArray
                  (IShape ())
                  (TFun ((TArray (IShape ((IVar wl (Some SNat)))) (TVar wt))
                         (TArray (IShape ()) TFloat)
                         (TArray (IShape ()) TFloat))
                        (TArray (IShape ((IVar wl (Some SNat)))) TFloat)))))
   (RExpr
    (ILam
     ((wl SNat))
     (RExpr
      (TLam
       (wt)
       (RExpr
        (Arr
         ()
         (#;<fn>
          (RElt
           (Lam
            ;; Maybe using '&' suffix to identify witness args is
            ;; a useful naming convention?
            ((length& (TArray (IShape ((IVar wl (Some SNat)))) (TVar wt)))
             (freq (TArray (IShape ()) TFloat))
             (phase (TArray (IShape ()) TFloat)))
            (RExpr
             (App
              (RExpr (Var cos))
              ((RExpr
                (App
                 (RExpr (Var +.))
                 ((RExpr (Var phase))
                  (RExpr
                   (App
                    (RExpr (Var *.))
                    (;; [0 1 2 ...]
                     (RExpr (App (RExpr (Var "float"))
                                 ((RExpr
                                   (App
                                    (RExpr
                                     (TApp
                                      (RExpr
                                       (IApp
                                        (RExpr (Var iota*))
                                        ((IShape ((IVar wl (Some SNat)))))))
                                      ((TVar wt))))
                                    ((RExpr (Var length&))))))))
                     (RExpr
                      (App
                       (RExpr (Var *.))
                       ((RExpr
                         (Var freq))
                        (RExpr
                         (Var tau))))))))))))))))))))))))
  (RDefn
   goertzel-iir-step
   (TArray
    (IShape ())
    (TFun ((TArray (IShape ()) TFloat))
          (TArray (IShape ())
                  (TFun ((TArray (IShape ()) TFloat)
                         (TArray (IShape ((INat 2))) TFloat))
                        (TArray (IShape ((INat 2))) TFloat)))))
   (RExpr
    (Arr
     ()
     ((RElt
       (Lam
        ((freq (TArray (IShape ()) TFloat)))
        (RExpr
         (Arr
          ()
          ((RElt
            (Lam
             ((next (TArray (IShape ()) TFloat))
              (accum (TArray (IShape ((INat 2))) TFloat)))
             (RExpr
              (Arr
               (2)
               ((RElt
                 (Expr
                  (RExpr
                   (App
                    (RExpr (Var -.))
                    ((RExpr
                      (App
                       (RExpr (Var +.))
                       ((RExpr (Var next))
                        (RExpr
                         (App
                          (RExpr (Var *.))
                          ((RExpr (Arr () ((RElt (Float 2.)))))
                           (RExpr
                            (App
                             (RExpr (Var *.))
                             ((RExpr
                               (App
                                (RExpr (Var cos))
                                ((RExpr
                                  (App
                                   (RExpr (Var *.))
                                   ((RExpr (Var tau))
                                    (RExpr (Var freq))))))))
                              (RExpr
                               (App
                                (RExpr
                                 (TApp
                                  (RExpr
                                   (IApp
                                    (RExpr (Var head))
                                    ((INat 1)
                                     (IShape ()))))
                                  (TFloat)))
                                ((RExpr (Var accum))))))))))))))
                     (RExpr
                      (App
                       (RExpr
                        (TApp
                         (RExpr
                          (IApp
                           (RExpr (Var tail))
                           ((INat 1)
                            (IShape ()))))
                         (TFloat)))
                       ((RExpr (Var accum))))))))))
                (RElt
                 (Expr
                  (RExpr
                   (App
                    (RExpr
                     (TApp
                      (RExpr
                       (IApp
                        (RExpr (Var head))
                        ((INat 1)
                         (IShape ()))))
                      (TFloat)))
                    ((RExpr (Var accum)))))))))))))))))))))
  (RDefn
   goertzel-iir
   (TDProd
    ((len SNat))
    (TArray (IShape ())
            (TFun ((TArray (IShape ()) TFloat)
                   (TArray (IShape ((IVar len (Some SNat)))) TFloat))
                  (TArray (IShape ((IVar len (Some SNat)))) TFloat))))
   (RExpr
    (ILam
     ((len SNat))
     (RExpr
      (Arr
       ()
       ((RElt
         (Lam
          ((freq (TArray (IShape ()) TFloat))
           (signal (TArray (IShape ((IVar len (Some SNat)))) TFloat)))
          (RExpr
           (App
            (RExpr
             (TApp
              (RExpr
               (IApp
                (RExpr (var head))
                ((INat 1)
                 (IShape ()))))
              (TFloat)))
            ((RExpr
              (App
               (RExpr
                (TApp
                 (RExpr
                  (IApp
                   (RExpr (Var scanl))
                   ((IVar len (Some SNat))
                    (IShape ((INat 2)))
                    (IShape ()))))
                 (TFloat TFloat)))
               ((RExpr (App (RExpr (Var goertzel-iir-step))
                            ((RExpr (Var freq)))))
                (RExpr (Arr (2) ((RElt (Float 0.)) (RElt (Float 0.)))))
                (RExpr (Var signal)))))))))))))))))
 ;; Main expression: Read chosen frequencies and signal, apply IIR stage
 ;; of Goertzel filters at those frequencies.
 (RExpr
  (App
   (RExpr
    (Arr
     ()
     ((RElt
       (Lam
        ((freq (TArray (IShape ()) TFloat)))
        (RExpr
         (Unpack
          (l) signal
          (RExpr (App (RExpr (Var readvec_f)) ()))
          (RExpr
           (Pack
            ((IVar l (Some SNat)))
            (RExpr
             (App
              (RExpr
               (IApp
                (RExpr (Var goertzel-iir))
                ((IVar l (Some SNat)))))
              ((RExpr (Var freq))
               (RExpr (Var signal)))))
            (TDSum ((sig-length SNat))
                   (TArray (IShape ((IVar sig-length (Some SNat))))
                           TFloat)))))))))))
   ((RExpr (App (RExpr (Var readscal_f)) ()))))))

