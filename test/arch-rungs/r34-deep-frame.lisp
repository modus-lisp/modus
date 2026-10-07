;;;; r34-deep-frame.lisp -- expect 42
;;;;
;;; A FRAME OF 240 SLOTS, ALL LIVE ACROSS A CALL.
;;;
;;; Frames are sized per function (FRAME-ENTER is TRAP #x8000|units<<8|nparams);
;;; before that every port reserved a fixed frame and the compiler refused
;;; anything past 120 bindings.  240 locals is past the old guard, past
;;; i386's old 57-slot and aarch64's old 121-slot frames, past RISC-V's 12-bit
;;; fp offset (slot 237 on rv64, so its slot accesses materialise the offset)
;;; and past 2 KB of frame (so the rv64 prologue cannot carve it with one ADDI).
;;;
;;; As in r25, the call in the middle is what makes a short frame visible: the
;;; callee's frame lands on any slot the caller did not reserve.  The AND reads
;;; every slot after the call returns.  It is EQL tests, not one 240-way `+':
;;; the generic-arithmetic chain is >32 KB of 68k code, past that back end's
;;; 16-bit branch reach, which is a code-size limit and not what this rung is
;;; about.

(defun bump (x) (+ x 1))

(defun probe ()
  (let* ((v0 0) (v1 1) (v2 2) (v3 3) (v4 4) (v5 5) (v6 6) (v7 7) (v8 8) (v9 9) (v10 10) (v11 11) (v12 12) (v13 13) (v14 14) (v15 15) (v16 16) (v17 17) (v18 18) (v19 19) (v20 20) (v21 21) (v22 22) (v23 23) (v24 24) (v25 25) (v26 26) (v27 27) (v28 28) (v29 29) (v30 30) (v31 31) (v32 32) (v33 33) (v34 34) (v35 35) (v36 36) (v37 37) (v38 38) (v39 39) (v40 40) (v41 41) (v42 42) (v43 43) (v44 44) (v45 45) (v46 46) (v47 47) (v48 48) (v49 49) (v50 50) (v51 51) (v52 52) (v53 53) (v54 54) (v55 55) (v56 56) (v57 57) (v58 58) (v59 59) (v60 60) (v61 61) (v62 62) (v63 63) (v64 64) (v65 65) (v66 66) (v67 67) (v68 68) (v69 69) (v70 70) (v71 71) (v72 72) (v73 73) (v74 74) (v75 75) (v76 76) (v77 77) (v78 78) (v79 79) (v80 80) (v81 81) (v82 82) (v83 83) (v84 84) (v85 85) (v86 86) (v87 87) (v88 88) (v89 89) (v90 90) (v91 91) (v92 92) (v93 93) (v94 94) (v95 95) (v96 96) (v97 97) (v98 98) (v99 99) (v100 100) (v101 101) (v102 102) (v103 103) (v104 104) (v105 105) (v106 106) (v107 107) (v108 108) (v109 109) (v110 110) (v111 111) (v112 112) (v113 113) (v114 114) (v115 115) (v116 116) (v117 117) (v118 118) (v119 119) (v120 120) (v121 121) (v122 122) (v123 123) (v124 124) (v125 125) (v126 126) (v127 127) (v128 128) (v129 129) (v130 130) (v131 131) (v132 132) (v133 133) (v134 134) (v135 135) (v136 136) (v137 137) (v138 138) (v139 139) (v140 140) (v141 141) (v142 142) (v143 143) (v144 144) (v145 145) (v146 146) (v147 147) (v148 148) (v149 149) (v150 150) (v151 151) (v152 152) (v153 153) (v154 154) (v155 155) (v156 156) (v157 157) (v158 158) (v159 159) (v160 160) (v161 161) (v162 162) (v163 163) (v164 164) (v165 165) (v166 166) (v167 167) (v168 168) (v169 169) (v170 170) (v171 171) (v172 172) (v173 173) (v174 174) (v175 175) (v176 176) (v177 177) (v178 178) (v179 179) (v180 180) (v181 181) (v182 182) (v183 183) (v184 184) (v185 185) (v186 186) (v187 187) (v188 188) (v189 189) (v190 190) (v191 191) (v192 192) (v193 193) (v194 194) (v195 195) (v196 196) (v197 197) (v198 198) (v199 199) (v200 200) (v201 201) (v202 202) (v203 203) (v204 204) (v205 205) (v206 206) (v207 207) (v208 208) (v209 209) (v210 210) (v211 211) (v212 212) (v213 213) (v214 214) (v215 215) (v216 216) (v217 217) (v218 218) (v219 219) (v220 220) (v221 221) (v222 222) (v223 223) (v224 224) (v225 225) (v226 226) (v227 227) (v228 228) (v229 229) (v230 230) (v231 231) (v232 232) (v233 233) (v234 234) (v235 235) (v236 236) (v237 237) (v238 238) (v239 239))
    (let ((q (bump 1)))
      (if (and (eql q 2)
               (eql v0 0) (eql v1 1) (eql v2 2) (eql v3 3) (eql v4 4) (eql v5 5) (eql v6 6) (eql v7 7)
               (eql v8 8) (eql v9 9) (eql v10 10) (eql v11 11) (eql v12 12) (eql v13 13) (eql v14 14) (eql v15 15)
               (eql v16 16) (eql v17 17) (eql v18 18) (eql v19 19) (eql v20 20) (eql v21 21) (eql v22 22) (eql v23 23)
               (eql v24 24) (eql v25 25) (eql v26 26) (eql v27 27) (eql v28 28) (eql v29 29) (eql v30 30) (eql v31 31)
               (eql v32 32) (eql v33 33) (eql v34 34) (eql v35 35) (eql v36 36) (eql v37 37) (eql v38 38) (eql v39 39)
               (eql v40 40) (eql v41 41) (eql v42 42) (eql v43 43) (eql v44 44) (eql v45 45) (eql v46 46) (eql v47 47)
               (eql v48 48) (eql v49 49) (eql v50 50) (eql v51 51) (eql v52 52) (eql v53 53) (eql v54 54) (eql v55 55)
               (eql v56 56) (eql v57 57) (eql v58 58) (eql v59 59) (eql v60 60) (eql v61 61) (eql v62 62) (eql v63 63)
               (eql v64 64) (eql v65 65) (eql v66 66) (eql v67 67) (eql v68 68) (eql v69 69) (eql v70 70) (eql v71 71)
               (eql v72 72) (eql v73 73) (eql v74 74) (eql v75 75) (eql v76 76) (eql v77 77) (eql v78 78) (eql v79 79)
               (eql v80 80) (eql v81 81) (eql v82 82) (eql v83 83) (eql v84 84) (eql v85 85) (eql v86 86) (eql v87 87)
               (eql v88 88) (eql v89 89) (eql v90 90) (eql v91 91) (eql v92 92) (eql v93 93) (eql v94 94) (eql v95 95)
               (eql v96 96) (eql v97 97) (eql v98 98) (eql v99 99) (eql v100 100) (eql v101 101) (eql v102 102) (eql v103 103)
               (eql v104 104) (eql v105 105) (eql v106 106) (eql v107 107) (eql v108 108) (eql v109 109) (eql v110 110) (eql v111 111)
               (eql v112 112) (eql v113 113) (eql v114 114) (eql v115 115) (eql v116 116) (eql v117 117) (eql v118 118) (eql v119 119)
               (eql v120 120) (eql v121 121) (eql v122 122) (eql v123 123) (eql v124 124) (eql v125 125) (eql v126 126) (eql v127 127)
               (eql v128 128) (eql v129 129) (eql v130 130) (eql v131 131) (eql v132 132) (eql v133 133) (eql v134 134) (eql v135 135)
               (eql v136 136) (eql v137 137) (eql v138 138) (eql v139 139) (eql v140 140) (eql v141 141) (eql v142 142) (eql v143 143)
               (eql v144 144) (eql v145 145) (eql v146 146) (eql v147 147) (eql v148 148) (eql v149 149) (eql v150 150) (eql v151 151)
               (eql v152 152) (eql v153 153) (eql v154 154) (eql v155 155) (eql v156 156) (eql v157 157) (eql v158 158) (eql v159 159)
               (eql v160 160) (eql v161 161) (eql v162 162) (eql v163 163) (eql v164 164) (eql v165 165) (eql v166 166) (eql v167 167)
               (eql v168 168) (eql v169 169) (eql v170 170) (eql v171 171) (eql v172 172) (eql v173 173) (eql v174 174) (eql v175 175)
               (eql v176 176) (eql v177 177) (eql v178 178) (eql v179 179) (eql v180 180) (eql v181 181) (eql v182 182) (eql v183 183)
               (eql v184 184) (eql v185 185) (eql v186 186) (eql v187 187) (eql v188 188) (eql v189 189) (eql v190 190) (eql v191 191)
               (eql v192 192) (eql v193 193) (eql v194 194) (eql v195 195) (eql v196 196) (eql v197 197) (eql v198 198) (eql v199 199)
               (eql v200 200) (eql v201 201) (eql v202 202) (eql v203 203) (eql v204 204) (eql v205 205) (eql v206 206) (eql v207 207)
               (eql v208 208) (eql v209 209) (eql v210 210) (eql v211 211) (eql v212 212) (eql v213 213) (eql v214 214) (eql v215 215)
               (eql v216 216) (eql v217 217) (eql v218 218) (eql v219 219) (eql v220 220) (eql v221 221) (eql v222 222) (eql v223 223)
               (eql v224 224) (eql v225 225) (eql v226 226) (eql v227 227) (eql v228 228) (eql v229 229) (eql v230 230) (eql v231 231)
               (eql v232 232) (eql v233 233) (eql v234 234) (eql v235 235) (eql v236 236) (eql v237 237) (eql v238 238) (eql v239 239))
          42
          0))))
