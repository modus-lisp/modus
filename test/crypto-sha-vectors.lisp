;;;; test/crypto-sha-vectors.lisp -- SHA-256 and SHA-512 against known digests.
;;;; Expected values are Python hashlib's.  Inputs: the byte pattern (i*7+3) mod 256,
;;;; or all zeros for the 4 MB case.  Covers every padding boundary (55..129), the
;;;; multi-block path, and a 4 MB input across ~65k blocks.
;;;; Run:  ./modus --load test/crypto-sha-vectors.lisp --eval '(sha-vectors-run)' --quit
;;;; The functions initialise their constants on demand, so no init call is needed.

(defun %sv-pattern (n)
  (let ((a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a) (setf (aref a i) (mod (+ (* i 7) 3) 256)))))
(defun %sv-zeros (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun %sv-hex (v n)
  (with-output-to-string (s) (dotimes (i n) (format s "~(~2,'0x~)" (aref v i)))))

(defparameter *sha-vectors*
  (list
  (list 0 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e")
  (list 1 "084fed08b978af4d7d196a7446a86b58009e636b611db16211b65a9aadff29c5" "e45bf5817ddf94aa2f7a407071f0eedc6beb98f768b4cd33d1176d44d1563a45a5d7212290eb7670c6786b13591aedac86478993895e8b24e612014abaa6ba04")
  (list 55 "e7313d333c272e639f790978283f9eb392e843d0f29b7016828bb1daa4aac70b" "14fd424b1fcadee624da946ab03f7e1def7c0d6e00f689594319881a26ff30b875ba4c622ac13100c8cc784c9c2eb23159aecbb4a02e3999062f551193e2b256")
  (list 56 "4324d65f3c103567f5589c710bc08f8523f929a9272e3af36fc968e52abc6c27" "480fa85be41ef55a41208ca28ffc8743c91cf7d24758defe6f95bfb16de614fc86b701034896b047dd571de4318853d80e0809df162f1752cb26da6ddb94a0dd")
  (list 63 "81c80242132f230c3bd41b3e63bbcff16107339549214a99614ff26664625055" "ecd42a703a4e93e163d60d55e3785b1a763838b0351bc2e6f7c94b4bfb24f9aa15da5d744ebcebe11f0fc4315d45ba3a047b6e60e07448357f2795bf34b73502")
  (list 64 "39e3d7b6b5d075d37d053ad89b24b41bef4f3c29760c84447cab3f3be1882241" "8f3cc30b3fb5bf963688a46488249248ac2c67f0f85a145233c6c1e3c16dcd1df634c07d1d31da02576f65b9cf64e1c3fdb318b689b8a14e2e9552bcf30fb133")
  (list 111 "67d9492e628fd376e0b2efec8ca2b99b123e202cf620deb270728df979b2f73e" "68cffa6d0d76f309c9ce0d35280939f8e25990c43b7b086ccdf709be35b07d4ddba599541ff2b1c19d34ea49aeafb9659adb7ac3c0b078bb30a22d57fc6687ef")
  (list 112 "96b928cff8528dbb99602c709a65b846cb6467acb8b722f0d758e4dc27bfc508" "d0865c524d1dddf7c23b799c413f5adcd7caefd3f66a9b49750ec81066012c25a8bcf94ddea6dc525691673097ca40e0101e897fc97218cfdb0704084e2bef4b")
  (list 127 "a8d23e75d936f303d248888d9b165ee543f4cbafcad3c9dd2a79bd84faa11d07" "e0b6a20f1c0c88970a9340152cd5a1c1ecf3d3b8de55102741879438079473540133b812706e5dbec322c8c9523b6fc8c6d16ee626e87ad5fe3d2916afedc369")
  (list 128 "d2742f1f4ac6bb7ca2b239ee18402ba8b3f9f8e652d2a72973c2b9ba11c08cf6" "99b16f17aa0b969a5b8f08f367719d516e330ccd2660b6f0688ec031dbc783de50a1cd185a2568dba75070a2403d17d4741d163578515dfd2ff756ddfe4d47b1")
  (list 129 "307f8fc2c1622b92762e818d39a185d4d667ad49a4b07ceae1f4afa008a93ec4" "a1556e29185778aa5991e34b8884c840d589f0fbb4b8ed590e51e9ac4eb03a008125000db2671f8fe7f485b59a77b518670078ecb41a54b4cd02a7f1d2ca4c6d")
  (list 1000 "1e9bc38cbf860b9ec31918b065f9b52476c549a782e0e7990bed8ce3868d2371" "00e36fccf193e59697a92b5ab24666ce6326d7fa16bf10832d0991ddc591112e9dfa6a636950ed9c4d67344a760654c2ff7785e1d60094d651038735b5dccabd")
  (list 65536 "510b126e1d4ced49107fe4ab03ee54cb1c8e4caf6064e1dd29c48d4a3e74c38b" "e926f618f10ac451ee87c850116725cee283ca5a6e9e50870ab6ec09290c1ce0b69ca299dd6c97607397d32126e10a34edd67ff5a6784c4aa6f7459fffc34bc2")
  (list 4194304 "bb9f8df61474d25e71fa00722318cd387396ca1736605e1248821cc0de3d3af8" "bd273bf4e10ed6e305ecb7b781cb065545fce9be9f1e2968df22c3a98f82d719855aafe5ff303d14ea623a5c55e51e924e10033a92a7a6b07725d7e9692b74f5"))
  )

(defun sha-vectors-run ()
  "Prints PASS/FAIL per case and a total.  Returns T if every case matched."
  (let ((pass 0) (fail 0))
    (dolist (c *sha-vectors*)
      (let* ((n (first c))
             (d (if (= n 4194304) (%sv-zeros n) (%sv-pattern n)))
             (ok256 (string= (%sv-hex (sha256 d) 32) (second c)))
             (ok512 (string= (%sv-hex (sha512 d) 64) (third c))))
        (if (and ok256 ok512)
            (progn (incf pass) (format t "PASS len ~D~%" n))
            (progn (incf fail) (format t "FAIL len ~D sha256=~A sha512=~A~%" n ok256 ok512)))))
    (format t "sha vectors: ~D passed, ~D failed~%" pass fail)
    (= fail 0)))
