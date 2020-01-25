;; Copyright (c) 2014-2020 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(with-test-bundle skip (test tap skip bundle)
  (no-plan)
  (format #t "not ok 1 - This should never be reached!~%"))
