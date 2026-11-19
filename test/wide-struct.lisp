;;;; test/wide-struct.lisp -- a structure with more slots than a call passes positionally (120).
;;;;   modus --script test/wide-struct.lisp   => WIDE-STRUCT PASS
;;;;
;;;; The internal constructor took one positional parameter per slot, and a call with more than 120
;;;; arguments passes 120 and then a list of the rest -- which only an &rest function accepts.  weft's
;;;; computed style (CSTYLE, 162 slots) could not be made.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defstruct wide (s0 0) (s1 1) (s2 2) (s3 3) (s4 4) (s5 5) (s6 6) (s7 7) (s8 8) (s9 9) (s10 10) (s11 11) (s12 12) (s13 13) (s14 14) (s15 15) (s16 16) (s17 17) (s18 18) (s19 19) (s20 20) (s21 21) (s22 22) (s23 23) (s24 24) (s25 25) (s26 26) (s27 27) (s28 28) (s29 29) (s30 30) (s31 31) (s32 32) (s33 33) (s34 34) (s35 35) (s36 36) (s37 37) (s38 38) (s39 39) (s40 40) (s41 41) (s42 42) (s43 43) (s44 44) (s45 45) (s46 46) (s47 47) (s48 48) (s49 49) (s50 50) (s51 51) (s52 52) (s53 53) (s54 54) (s55 55) (s56 56) (s57 57) (s58 58) (s59 59) (s60 60) (s61 61) (s62 62) (s63 63) (s64 64) (s65 65) (s66 66) (s67 67) (s68 68) (s69 69) (s70 70) (s71 71) (s72 72) (s73 73) (s74 74) (s75 75) (s76 76) (s77 77) (s78 78) (s79 79) (s80 80) (s81 81) (s82 82) (s83 83) (s84 84) (s85 85) (s86 86) (s87 87) (s88 88) (s89 89) (s90 90) (s91 91) (s92 92) (s93 93) (s94 94) (s95 95) (s96 96) (s97 97) (s98 98) (s99 99) (s100 100) (s101 101) (s102 102) (s103 103) (s104 104) (s105 105) (s106 106) (s107 107) (s108 108) (s109 109) (s110 110) (s111 111) (s112 112) (s113 113) (s114 114) (s115 115) (s116 116) (s117 117) (s118 118) (s119 119) (s120 120) (s121 121) (s122 122) (s123 123) (s124 124) (s125 125) (s126 126) (s127 127) (s128 128) (s129 129) (s130 130) (s131 131) (s132 132) (s133 133) (s134 134) (s135 135) (s136 136) (s137 137) (s138 138) (s139 139) (s140 140) (s141 141) (s142 142) (s143 143) (s144 144) (s145 145) (s146 146) (s147 147) (s148 148) (s149 149))
(defun mk () (make-wide :s0 :first :s149 :last :s120 :mid))
(let ((w (ignore-errors (mk))))
  (check "made" (and w t) t)
  (when w
    (check "the first slot" (wide-s0 w) :first)
    (check "a defaulted slot" (wide-s7 w) 7)
    (check "past 120" (wide-s120 w) :mid)
    (check "the last" (wide-s149 w) :last)
    (check "a copy" (wide-s149 (copy-wide w)) :last)
    (setf (wide-s130 w) :set)
    (check "set past 120" (wide-s130 w) :set)))
(format t "~&WIDE-STRUCT ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
