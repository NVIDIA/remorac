(RProg
 ;; Top-level definitions
 (#;<defn>
  (RDefn sample-rate (TArray (IShape ()) TFloat)
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
                  (TFun ((TArray (IShape ((IVar wl))) (TVar wt))
                         (TArray (IShape ()) TFloat)
                         (TArray (IShape ()) TFloat))
                        (TArray (IShape ((IVar wl))) TFloat)))))
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
            ((length& (TArray (IShape ((IVar wl))) (TVar wt)))
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
                                        ((IShape ((IVar wl))))))
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
                        (TArray (IShape ((INat 2)))
                                (TArray (IShape ()) TFloat))))))
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
  #;<more-defns>
  )
  ;; Main expression
  (RExpr
   (App
    (RExpr
     (TApp
      (RExpr
       (IApp
        (RExpr
         (Var sinusoid*))
        ((INat 10))))
      (TInt)))
    ((RExpr (Arr (10) ((RElt (Int 0)) (RElt (Int 0))
                       (RElt (Int 0)) (RElt (Int 0))
                       (RElt (Int 0)) (RElt (Int 0))
                       (RElt (Int 0)) (RElt (Int 0))
                       (RElt (Int 0)) (RElt (Int 0)))))
     (RExpr (Arr () ((RElt (Float 0.25)))))
     (RExpr (Arr () ((RElt (Float 0.)))))))))

