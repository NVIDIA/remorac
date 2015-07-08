;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 2015, NVIDIA CORPORATION. All rights reserved.               ;;
;;                                                                            ;;
;; Redistribution and use in source and binary forms, with or without         ;;
;; modification, are permitted provided that the following conditions         ;;
;; are met:                                                                   ;;
;;  * Redistributions of source code must retain the above copyright          ;;
;;    notice, this list of conditions and the following disclaimer.           ;;
;;  * Redistributions in binary form must reproduce the above copyright       ;;
;;    notice, this list of conditions and the following disclaimer in the     ;;
;;    documentation and/or other materials provided with the distribution.    ;;
;;  * Neither the name of NVIDIA CORPORATION nor the names of its             ;;
;;    contributors may be used to endorse or promote products derived         ;;
;;    from this software without specific prior written permission.           ;;
;;                                                                            ;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY       ;;
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE          ;;
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR         ;;
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR          ;;
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,      ;;
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,        ;;
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR         ;;
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY        ;;
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT               ;;
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE      ;;
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(RProg
 ((RDefn
   lerp
   (TArray (IShape ())
           (TFun ((TArray (IShape ()) TFloat)
                  (TArray (IShape ()) TFloat)
                  (TArray (IShape ()) TFloat))
                 (TArray (IShape ()) TFloat)))
   (RExpr
    (Arr ()
         ((RElt
           (Lam ((lo (TArray (IShape ()) TFloat))
                 (hi (TArray (IShape ()) TFloat))
                 (mid (TArray (IShape ()) TFloat)))
                (RExpr
                 (App (RExpr (Var +.))
                      ((RExpr (App (RExpr (Var *.))
                                   ((RExpr (Var mid))
                                    (RExpr (Var hi)))))
                       (RExpr (App (RExpr (Var *.))
                                   ((RExpr (App
                                            (RExpr (Var -.))
                                            ((RExpr
                                              (Arr ()
                                                   ((RElt (Float 1.0)))))
                                             (RExpr (Var mid)))))
                                    (RExpr (Var lo)))))))))))))))
  (RExpr (App (RExpr (Var lerp))
             ((RExpr (Arr (2) ((RElt (Float 0.0)) (RElt (Float 1.0)))))
              (RExpr (Arr (2) ((RElt (Float 7.0)) (RElt (Float 4.0)))))
              (RExpr (Arr () ((RElt (Float 0.5)))))))))
