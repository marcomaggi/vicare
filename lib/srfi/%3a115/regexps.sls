;; regexp.scm -- simple non-bactracking NFA implementation
;; Copyright (c) 2013-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; A regular expression engine implementing SRFI 115 using a
;; non-backtracking Thompson NFA algorithm.


(library (srfi :115 regexps)
  (export
    regexp regexp? valid-sre? rx regexp->sre char-set->sre
    regexp-matches regexp-matches? regexp-search
    regexp-replace regexp-replace-all
    regexp-fold regexp-extract regexp-split regexp-partition
    regexp-match? regexp-match-count
    regexp-match-submatch regexp-match-submatch/list
    regexp-match-submatch-start regexp-match-submatch-end
    regexp-match->list regexp-match->sexp)
  (import (vicare)
    (srfi :14))


;;;; character sets

(define %char-set:letter
  (char-set-intersection char-set:ascii char-set:letter))
(define %char-set:lower-case
  (char-set-intersection char-set:ascii char-set:lower-case))
(define %char-set:upper-case
  (char-set-intersection char-set:ascii char-set:upper-case))
(define %char-set:digit
  (char-set-intersection char-set:ascii char-set:digit))
(define %char-set:letter+digit
  (char-set-intersection char-set:ascii char-set:letter+digit))
(define %char-set:punctuation
  (char-set-intersection char-set:ascii char-set:punctuation))
(define %char-set:symbol
  (char-set-intersection char-set:ascii char-set:symbol))
(define %char-set:graphic
  (char-set-intersection char-set:ascii char-set:graphic))
(define %char-set:whitespace
  (char-set-intersection char-set:ascii char-set:whitespace))
(define %char-set:printing
  (char-set-intersection char-set:ascii char-set:printing))
(define %char-set:iso-control
  (char-set-intersection char-set:ascii char-set:iso-control))


;;;; char sets boundary

(define (immutable-char-set obj)
  obj)

;; Control
#;(define char-set:control (immutable-char-set (char-set-union (ucs-range->char-set 0 10) (ucs-range->char-set 11 13) (ucs-range->char-set 14 32) (ucs-range->char-set 127 160) (ucs-range->char-set 1536 1541) (ucs-range->char-set 8206 8208) (ucs-range->char-set 8234 8239) (ucs-range->char-set 8288 8293) (ucs-range->char-set 8294 8304) (ucs-range->char-set 55296 57344) (ucs-range->char-set 65520 65529) (ucs-range->char-set 65529 65532) (ucs-range->char-set 119155 119163) (ucs-range->char-set 917506 917536) (ucs-range->char-set 917536 917632) (ucs-range->char-set 917632 917760) (ucs-range->char-set 918000 921600))))

;; Extend,SpacingMark
(define char-set:extend-or-spacing-mark (immutable-char-set (char-set-union (char-set-union (ucs-range->char-set 768 880) (ucs-range->char-set 1155 1160) (ucs-range->char-set 1160 1162) (ucs-range->char-set 1425 1470) (ucs-range->char-set 1473 1475) (ucs-range->char-set 1476 1478) (ucs-range->char-set 1552 1563) (ucs-range->char-set 1611 1632) (ucs-range->char-set 1750 1757) (ucs-range->char-set 1759 1765) (ucs-range->char-set 1767 1769) (ucs-range->char-set 1770 1774) (ucs-range->char-set 1840 1867) (ucs-range->char-set 1958 1969) (ucs-range->char-set 2027 2036) (ucs-range->char-set 2070 2074) (ucs-range->char-set 2075 2084) (ucs-range->char-set 2085 2088) (ucs-range->char-set 2089 2094) (ucs-range->char-set 2137 2140) (ucs-range->char-set 2276 2303) (ucs-range->char-set 2304 2307) (ucs-range->char-set 2369 2377) (ucs-range->char-set 2385 2392) (ucs-range->char-set 2402 2404) (ucs-range->char-set 2497 2501) (ucs-range->char-set 2530 2532) (ucs-range->char-set 2561 2563) (ucs-range->char-set 2625 2627) (ucs-range->char-set 2631 2633) (ucs-range->char-set 2635 2638) (ucs-range->char-set 2672 2674) (ucs-range->char-set 2689 2691) (ucs-range->char-set 2753 2758) (ucs-range->char-set 2759 2761) (ucs-range->char-set 2786 2788) (ucs-range->char-set 2881 2885) (ucs-range->char-set 2914 2916) (ucs-range->char-set 3134 3137) (ucs-range->char-set 3142 3145) (ucs-range->char-set 3146 3150) (ucs-range->char-set 3157 3159) (ucs-range->char-set 3170 3172) (ucs-range->char-set 3276 3278) (ucs-range->char-set 3285 3287) (ucs-range->char-set 3298 3300) (ucs-range->char-set 3393 3397) (ucs-range->char-set 3426 3428) (ucs-range->char-set 3538 3541) (ucs-range->char-set 3636 3643) (ucs-range->char-set 3655 3663) (ucs-range->char-set 3764 3770) (ucs-range->char-set 3771 3773) (ucs-range->char-set 3784 3790) (ucs-range->char-set 3864 3866) (ucs-range->char-set 3953 3967) (ucs-range->char-set 3968 3973) (ucs-range->char-set 3974 3976) (ucs-range->char-set 3981 3992) (ucs-range->char-set 3993 4029) (ucs-range->char-set 4141 4145) (ucs-range->char-set 4146 4152) (ucs-range->char-set 4153 4155) (ucs-range->char-set 4157 4159) (ucs-range->char-set 4184 4186) (ucs-range->char-set 4190 4193) (ucs-range->char-set 4209 4213) (ucs-range->char-set 4229 4231) (ucs-range->char-set 4957 4960) (ucs-range->char-set 5906 5909) (ucs-range->char-set 5938 5941) (ucs-range->char-set 5970 5972) (ucs-range->char-set 6002 6004) (ucs-range->char-set 6068 6070) (ucs-range->char-set 6071 6078) (ucs-range->char-set 6089 6100) (ucs-range->char-set 6155 6158) (ucs-range->char-set 6432 6435) (ucs-range->char-set 6439 6441) (ucs-range->char-set 6457 6460) (ucs-range->char-set 6679 6681) (ucs-range->char-set 6744 6751) (ucs-range->char-set 6757 6765) (ucs-range->char-set 6771 6781) (ucs-range->char-set 6912 6916) (ucs-range->char-set 6966 6971) (ucs-range->char-set 7019 7028) (ucs-range->char-set 7040 7042) (ucs-range->char-set 7074 7078) (ucs-range->char-set 7080 7082) (ucs-range->char-set 7144 7146) (ucs-range->char-set 7151 7154) (ucs-range->char-set 7212 7220) (ucs-range->char-set 7222 7224) (ucs-range->char-set 7376 7379) (ucs-range->char-set 7380 7393) (ucs-range->char-set 7394 7401) (ucs-range->char-set 7616 7655) (ucs-range->char-set 7676 7680) (ucs-range->char-set 8204 8206) (ucs-range->char-set 8400 8413) (ucs-range->char-set 8413 8417) (ucs-range->char-set 8418 8421) (ucs-range->char-set 8421 8433) (ucs-range->char-set 11503 11506) (ucs-range->char-set 11744 11776) (ucs-range->char-set 12330 12334) (ucs-range->char-set 12334 12336) (ucs-range->char-set 12441 12443) (ucs-range->char-set 42608 42611) (ucs-range->char-set 42612 42622) (ucs-range->char-set 42736 42738) (ucs-range->char-set 43045 43047) (ucs-range->char-set 43232 43250) (ucs-range->char-set 43302 43310) (ucs-range->char-set 43335 43346) (ucs-range->char-set 43392 43395) (ucs-range->char-set 43446 43450) (ucs-range->char-set 43561 43567) (ucs-range->char-set 43569 43571) (ucs-range->char-set 43573 43575) (ucs-range->char-set 43698 43701) (ucs-range->char-set 43703 43705) (ucs-range->char-set 43710 43712) (ucs-range->char-set 43756 43758) (ucs-range->char-set 65024 65040) (ucs-range->char-set 65056 65063) (ucs-range->char-set 65438 65440) (ucs-range->char-set 68097 68100) (ucs-range->char-set 68101 68103) (ucs-range->char-set 68108 68112) (ucs-range->char-set 68152 68155) (ucs-range->char-set 69688 69703) (ucs-range->char-set 69760 69762) (ucs-range->char-set 69811 69815) (ucs-range->char-set 69817 69819) (ucs-range->char-set 69888 69891) (ucs-range->char-set 69927 69932) (ucs-range->char-set 69933 69941) (ucs-range->char-set 70016 70018) (ucs-range->char-set 70070 70079) (ucs-range->char-set 71344 71350) (ucs-range->char-set 94095 94099) (ucs-range->char-set 119143 119146) (ucs-range->char-set 119150 119155) (ucs-range->char-set 119163 119171) (ucs-range->char-set 119173 119180) (ucs-range->char-set 119210 119214) (ucs-range->char-set 119362 119365) (ucs-range->char-set 917760 918000)) (char-set-union (ucs-range->char-set 2366 2369) (ucs-range->char-set 2377 2381) (ucs-range->char-set 2382 2384) (ucs-range->char-set 2434 2436) (ucs-range->char-set 2495 2497) (ucs-range->char-set 2503 2505) (ucs-range->char-set 2507 2509) (ucs-range->char-set 2622 2625) (ucs-range->char-set 2750 2753) (ucs-range->char-set 2763 2765) (ucs-range->char-set 2818 2820) (ucs-range->char-set 2887 2889) (ucs-range->char-set 2891 2893) (ucs-range->char-set 3009 3011) (ucs-range->char-set 3014 3017) (ucs-range->char-set 3018 3021) (ucs-range->char-set 3073 3076) (ucs-range->char-set 3137 3141) (ucs-range->char-set 3202 3204) (ucs-range->char-set 3264 3266) (ucs-range->char-set 3267 3269) (ucs-range->char-set 3271 3273) (ucs-range->char-set 3274 3276) (ucs-range->char-set 3330 3332) (ucs-range->char-set 3391 3393) (ucs-range->char-set 3398 3401) (ucs-range->char-set 3402 3405) (ucs-range->char-set 3458 3460) (ucs-range->char-set 3536 3538) (ucs-range->char-set 3544 3551) (ucs-range->char-set 3570 3572) (ucs-range->char-set 3902 3904) (ucs-range->char-set 4155 4157) (ucs-range->char-set 4182 4184) (ucs-range->char-set 6078 6086) (ucs-range->char-set 6087 6089) (ucs-range->char-set 6435 6439) (ucs-range->char-set 6441 6444) (ucs-range->char-set 6448 6450) (ucs-range->char-set 6451 6457) (ucs-range->char-set 6581 6584) (ucs-range->char-set 6681 6683) (ucs-range->char-set 6765 6771) (ucs-range->char-set 6973 6978) (ucs-range->char-set 6979 6981) (ucs-range->char-set 7078 7080) (ucs-range->char-set 7084 7086) (ucs-range->char-set 7146 7149) (ucs-range->char-set 7154 7156) (ucs-range->char-set 7204 7212) (ucs-range->char-set 7220 7222) (ucs-range->char-set 7410 7412) (ucs-range->char-set 43043 43045) (ucs-range->char-set 43136 43138) (ucs-range->char-set 43188 43204) (ucs-range->char-set 43346 43348) (ucs-range->char-set 43444 43446) (ucs-range->char-set 43450 43452) (ucs-range->char-set 43453 43457) (ucs-range->char-set 43567 43569) (ucs-range->char-set 43571 43573) (ucs-range->char-set 43758 43760) (ucs-range->char-set 44003 44005) (ucs-range->char-set 44006 44008) (ucs-range->char-set 44009 44011) (ucs-range->char-set 69808 69811) (ucs-range->char-set 69815 69817) (ucs-range->char-set 70067 70070) (ucs-range->char-set 70079 70081) (ucs-range->char-set 71342 71344) (ucs-range->char-set 94033 94079)))))

;; Regional_Indicator
(define char-set:regional-indicator (immutable-char-set (char-set-union (ucs-range->char-set 127462 127488))))

;; :L
(define char-set:hangul-l (immutable-char-set (char-set-union (ucs-range->char-set 4352 4448) (ucs-range->char-set 43360 43389))))

;; :V
(define char-set:hangul-v (immutable-char-set (char-set-union (ucs-range->char-set 4448 4520) (ucs-range->char-set 55216 55239))))

;; :T
(define char-set:hangul-t (immutable-char-set (char-set-union (ucs-range->char-set 4520 4608) (ucs-range->char-set 55243 55292))))

;; :LV
(define char-set:hangul-lv (immutable-char-set (char-set-union)))

;; :LVT
(define char-set:hangul-lvt (immutable-char-set (char-set-union (ucs-range->char-set 44033 44060) (ucs-range->char-set 44061 44088) (ucs-range->char-set 44089 44116) (ucs-range->char-set 44117 44144) (ucs-range->char-set 44145 44172) (ucs-range->char-set 44173 44200) (ucs-range->char-set 44201 44228) (ucs-range->char-set 44229 44256) (ucs-range->char-set 44257 44284) (ucs-range->char-set 44285 44312) (ucs-range->char-set 44313 44340) (ucs-range->char-set 44341 44368) (ucs-range->char-set 44369 44396) (ucs-range->char-set 44397 44424) (ucs-range->char-set 44425 44452) (ucs-range->char-set 44453 44480) (ucs-range->char-set 44481 44508) (ucs-range->char-set 44509 44536) (ucs-range->char-set 44537 44564) (ucs-range->char-set 44565 44592) (ucs-range->char-set 44593 44620) (ucs-range->char-set 44621 44648) (ucs-range->char-set 44649 44676) (ucs-range->char-set 44677 44704) (ucs-range->char-set 44705 44732) (ucs-range->char-set 44733 44760) (ucs-range->char-set 44761 44788) (ucs-range->char-set 44789 44816) (ucs-range->char-set 44817 44844) (ucs-range->char-set 44845 44872) (ucs-range->char-set 44873 44900) (ucs-range->char-set 44901 44928) (ucs-range->char-set 44929 44956) (ucs-range->char-set 44957 44984) (ucs-range->char-set 44985 45012) (ucs-range->char-set 45013 45040) (ucs-range->char-set 45041 45068) (ucs-range->char-set 45069 45096) (ucs-range->char-set 45097 45124) (ucs-range->char-set 45125 45152) (ucs-range->char-set 45153 45180) (ucs-range->char-set 45181 45208) (ucs-range->char-set 45209 45236) (ucs-range->char-set 45237 45264) (ucs-range->char-set 45265 45292) (ucs-range->char-set 45293 45320) (ucs-range->char-set 45321 45348) (ucs-range->char-set 45349 45376) (ucs-range->char-set 45377 45404) (ucs-range->char-set 45405 45432) (ucs-range->char-set 45433 45460) (ucs-range->char-set 45461 45488) (ucs-range->char-set 45489 45516) (ucs-range->char-set 45517 45544) (ucs-range->char-set 45545 45572) (ucs-range->char-set 45573 45600) (ucs-range->char-set 45601 45628) (ucs-range->char-set 45629 45656) (ucs-range->char-set 45657 45684) (ucs-range->char-set 45685 45712) (ucs-range->char-set 45713 45740) (ucs-range->char-set 45741 45768) (ucs-range->char-set 45769 45796) (ucs-range->char-set 45797 45824) (ucs-range->char-set 45825 45852) (ucs-range->char-set 45853 45880) (ucs-range->char-set 45881 45908) (ucs-range->char-set 45909 45936) (ucs-range->char-set 45937 45964) (ucs-range->char-set 45965 45992) (ucs-range->char-set 45993 46020) (ucs-range->char-set 46021 46048) (ucs-range->char-set 46049 46076) (ucs-range->char-set 46077 46104) (ucs-range->char-set 46105 46132) (ucs-range->char-set 46133 46160) (ucs-range->char-set 46161 46188) (ucs-range->char-set 46189 46216) (ucs-range->char-set 46217 46244) (ucs-range->char-set 46245 46272) (ucs-range->char-set 46273 46300) (ucs-range->char-set 46301 46328) (ucs-range->char-set 46329 46356) (ucs-range->char-set 46357 46384) (ucs-range->char-set 46385 46412) (ucs-range->char-set 46413 46440) (ucs-range->char-set 46441 46468) (ucs-range->char-set 46469 46496) (ucs-range->char-set 46497 46524) (ucs-range->char-set 46525 46552) (ucs-range->char-set 46553 46580) (ucs-range->char-set 46581 46608) (ucs-range->char-set 46609 46636) (ucs-range->char-set 46637 46664) (ucs-range->char-set 46665 46692) (ucs-range->char-set 46693 46720) (ucs-range->char-set 46721 46748) (ucs-range->char-set 46749 46776) (ucs-range->char-set 46777 46804) (ucs-range->char-set 46805 46832) (ucs-range->char-set 46833 46860) (ucs-range->char-set 46861 46888) (ucs-range->char-set 46889 46916) (ucs-range->char-set 46917 46944) (ucs-range->char-set 46945 46972) (ucs-range->char-set 46973 47000) (ucs-range->char-set 47001 47028) (ucs-range->char-set 47029 47056) (ucs-range->char-set 47057 47084) (ucs-range->char-set 47085 47112) (ucs-range->char-set 47113 47140) (ucs-range->char-set 47141 47168) (ucs-range->char-set 47169 47196) (ucs-range->char-set 47197 47224) (ucs-range->char-set 47225 47252) (ucs-range->char-set 47253 47280) (ucs-range->char-set 47281 47308) (ucs-range->char-set 47309 47336) (ucs-range->char-set 47337 47364) (ucs-range->char-set 47365 47392) (ucs-range->char-set 47393 47420) (ucs-range->char-set 47421 47448) (ucs-range->char-set 47449 47476) (ucs-range->char-set 47477 47504) (ucs-range->char-set 47505 47532) (ucs-range->char-set 47533 47560) (ucs-range->char-set 47561 47588) (ucs-range->char-set 47589 47616) (ucs-range->char-set 47617 47644) (ucs-range->char-set 47645 47672) (ucs-range->char-set 47673 47700) (ucs-range->char-set 47701 47728) (ucs-range->char-set 47729 47756) (ucs-range->char-set 47757 47784) (ucs-range->char-set 47785 47812) (ucs-range->char-set 47813 47840) (ucs-range->char-set 47841 47868) (ucs-range->char-set 47869 47896) (ucs-range->char-set 47897 47924) (ucs-range->char-set 47925 47952) (ucs-range->char-set 47953 47980) (ucs-range->char-set 47981 48008) (ucs-range->char-set 48009 48036) (ucs-range->char-set 48037 48064) (ucs-range->char-set 48065 48092) (ucs-range->char-set 48093 48120) (ucs-range->char-set 48121 48148) (ucs-range->char-set 48149 48176) (ucs-range->char-set 48177 48204) (ucs-range->char-set 48205 48232) (ucs-range->char-set 48233 48260) (ucs-range->char-set 48261 48288) (ucs-range->char-set 48289 48316) (ucs-range->char-set 48317 48344) (ucs-range->char-set 48345 48372) (ucs-range->char-set 48373 48400) (ucs-range->char-set 48401 48428) (ucs-range->char-set 48429 48456) (ucs-range->char-set 48457 48484) (ucs-range->char-set 48485 48512) (ucs-range->char-set 48513 48540) (ucs-range->char-set 48541 48568) (ucs-range->char-set 48569 48596) (ucs-range->char-set 48597 48624) (ucs-range->char-set 48625 48652) (ucs-range->char-set 48653 48680) (ucs-range->char-set 48681 48708) (ucs-range->char-set 48709 48736) (ucs-range->char-set 48737 48764) (ucs-range->char-set 48765 48792) (ucs-range->char-set 48793 48820) (ucs-range->char-set 48821 48848) (ucs-range->char-set 48849 48876) (ucs-range->char-set 48877 48904) (ucs-range->char-set 48905 48932) (ucs-range->char-set 48933 48960) (ucs-range->char-set 48961 48988) (ucs-range->char-set 48989 49016) (ucs-range->char-set 49017 49044) (ucs-range->char-set 49045 49072) (ucs-range->char-set 49073 49100) (ucs-range->char-set 49101 49128) (ucs-range->char-set 49129 49156) (ucs-range->char-set 49157 49184) (ucs-range->char-set 49185 49212) (ucs-range->char-set 49213 49240) (ucs-range->char-set 49241 49268) (ucs-range->char-set 49269 49296) (ucs-range->char-set 49297 49324) (ucs-range->char-set 49325 49352) (ucs-range->char-set 49353 49380) (ucs-range->char-set 49381 49408) (ucs-range->char-set 49409 49436) (ucs-range->char-set 49437 49464) (ucs-range->char-set 49465 49492) (ucs-range->char-set 49493 49520) (ucs-range->char-set 49521 49548) (ucs-range->char-set 49549 49576) (ucs-range->char-set 49577 49604) (ucs-range->char-set 49605 49632) (ucs-range->char-set 49633 49660) (ucs-range->char-set 49661 49688) (ucs-range->char-set 49689 49716) (ucs-range->char-set 49717 49744) (ucs-range->char-set 49745 49772) (ucs-range->char-set 49773 49800) (ucs-range->char-set 49801 49828) (ucs-range->char-set 49829 49856) (ucs-range->char-set 49857 49884) (ucs-range->char-set 49885 49912) (ucs-range->char-set 49913 49940) (ucs-range->char-set 49941 49968) (ucs-range->char-set 49969 49996) (ucs-range->char-set 49997 50024) (ucs-range->char-set 50025 50052) (ucs-range->char-set 50053 50080) (ucs-range->char-set 50081 50108) (ucs-range->char-set 50109 50136) (ucs-range->char-set 50137 50164) (ucs-range->char-set 50165 50192) (ucs-range->char-set 50193 50220) (ucs-range->char-set 50221 50248) (ucs-range->char-set 50249 50276) (ucs-range->char-set 50277 50304) (ucs-range->char-set 50305 50332) (ucs-range->char-set 50333 50360) (ucs-range->char-set 50361 50388) (ucs-range->char-set 50389 50416) (ucs-range->char-set 50417 50444) (ucs-range->char-set 50445 50472) (ucs-range->char-set 50473 50500) (ucs-range->char-set 50501 50528) (ucs-range->char-set 50529 50556) (ucs-range->char-set 50557 50584) (ucs-range->char-set 50585 50612) (ucs-range->char-set 50613 50640) (ucs-range->char-set 50641 50668) (ucs-range->char-set 50669 50696) (ucs-range->char-set 50697 50724) (ucs-range->char-set 50725 50752) (ucs-range->char-set 50753 50780) (ucs-range->char-set 50781 50808) (ucs-range->char-set 50809 50836) (ucs-range->char-set 50837 50864) (ucs-range->char-set 50865 50892) (ucs-range->char-set 50893 50920) (ucs-range->char-set 50921 50948) (ucs-range->char-set 50949 50976) (ucs-range->char-set 50977 51004) (ucs-range->char-set 51005 51032) (ucs-range->char-set 51033 51060) (ucs-range->char-set 51061 51088) (ucs-range->char-set 51089 51116) (ucs-range->char-set 51117 51144) (ucs-range->char-set 51145 51172) (ucs-range->char-set 51173 51200) (ucs-range->char-set 51201 51228) (ucs-range->char-set 51229 51256) (ucs-range->char-set 51257 51284) (ucs-range->char-set 51285 51312) (ucs-range->char-set 51313 51340) (ucs-range->char-set 51341 51368) (ucs-range->char-set 51369 51396) (ucs-range->char-set 51397 51424) (ucs-range->char-set 51425 51452) (ucs-range->char-set 51453 51480) (ucs-range->char-set 51481 51508) (ucs-range->char-set 51509 51536) (ucs-range->char-set 51537 51564) (ucs-range->char-set 51565 51592) (ucs-range->char-set 51593 51620) (ucs-range->char-set 51621 51648) (ucs-range->char-set 51649 51676) (ucs-range->char-set 51677 51704) (ucs-range->char-set 51705 51732) (ucs-range->char-set 51733 51760) (ucs-range->char-set 51761 51788) (ucs-range->char-set 51789 51816) (ucs-range->char-set 51817 51844) (ucs-range->char-set 51845 51872) (ucs-range->char-set 51873 51900) (ucs-range->char-set 51901 51928) (ucs-range->char-set 51929 51956) (ucs-range->char-set 51957 51984) (ucs-range->char-set 51985 52012) (ucs-range->char-set 52013 52040) (ucs-range->char-set 52041 52068) (ucs-range->char-set 52069 52096) (ucs-range->char-set 52097 52124) (ucs-range->char-set 52125 52152) (ucs-range->char-set 52153 52180) (ucs-range->char-set 52181 52208) (ucs-range->char-set 52209 52236) (ucs-range->char-set 52237 52264) (ucs-range->char-set 52265 52292) (ucs-range->char-set 52293 52320) (ucs-range->char-set 52321 52348) (ucs-range->char-set 52349 52376) (ucs-range->char-set 52377 52404) (ucs-range->char-set 52405 52432) (ucs-range->char-set 52433 52460) (ucs-range->char-set 52461 52488) (ucs-range->char-set 52489 52516) (ucs-range->char-set 52517 52544) (ucs-range->char-set 52545 52572) (ucs-range->char-set 52573 52600) (ucs-range->char-set 52601 52628) (ucs-range->char-set 52629 52656) (ucs-range->char-set 52657 52684) (ucs-range->char-set 52685 52712) (ucs-range->char-set 52713 52740) (ucs-range->char-set 52741 52768) (ucs-range->char-set 52769 52796) (ucs-range->char-set 52797 52824) (ucs-range->char-set 52825 52852) (ucs-range->char-set 52853 52880) (ucs-range->char-set 52881 52908) (ucs-range->char-set 52909 52936) (ucs-range->char-set 52937 52964) (ucs-range->char-set 52965 52992) (ucs-range->char-set 52993 53020) (ucs-range->char-set 53021 53048) (ucs-range->char-set 53049 53076) (ucs-range->char-set 53077 53104) (ucs-range->char-set 53105 53132) (ucs-range->char-set 53133 53160) (ucs-range->char-set 53161 53188) (ucs-range->char-set 53189 53216) (ucs-range->char-set 53217 53244) (ucs-range->char-set 53245 53272) (ucs-range->char-set 53273 53300) (ucs-range->char-set 53301 53328) (ucs-range->char-set 53329 53356) (ucs-range->char-set 53357 53384) (ucs-range->char-set 53385 53412) (ucs-range->char-set 53413 53440) (ucs-range->char-set 53441 53468) (ucs-range->char-set 53469 53496) (ucs-range->char-set 53497 53524) (ucs-range->char-set 53525 53552) (ucs-range->char-set 53553 53580) (ucs-range->char-set 53581 53608) (ucs-range->char-set 53609 53636) (ucs-range->char-set 53637 53664) (ucs-range->char-set 53665 53692) (ucs-range->char-set 53693 53720) (ucs-range->char-set 53721 53748) (ucs-range->char-set 53749 53776) (ucs-range->char-set 53777 53804) (ucs-range->char-set 53805 53832) (ucs-range->char-set 53833 53860) (ucs-range->char-set 53861 53888) (ucs-range->char-set 53889 53916) (ucs-range->char-set 53917 53944) (ucs-range->char-set 53945 53972) (ucs-range->char-set 53973 54000) (ucs-range->char-set 54001 54028) (ucs-range->char-set 54029 54056) (ucs-range->char-set 54057 54084) (ucs-range->char-set 54085 54112) (ucs-range->char-set 54113 54140) (ucs-range->char-set 54141 54168) (ucs-range->char-set 54169 54196) (ucs-range->char-set 54197 54224) (ucs-range->char-set 54225 54252) (ucs-range->char-set 54253 54280) (ucs-range->char-set 54281 54308) (ucs-range->char-set 54309 54336) (ucs-range->char-set 54337 54364) (ucs-range->char-set 54365 54392) (ucs-range->char-set 54393 54420) (ucs-range->char-set 54421 54448) (ucs-range->char-set 54449 54476) (ucs-range->char-set 54477 54504) (ucs-range->char-set 54505 54532) (ucs-range->char-set 54533 54560) (ucs-range->char-set 54561 54588) (ucs-range->char-set 54589 54616) (ucs-range->char-set 54617 54644) (ucs-range->char-set 54645 54672) (ucs-range->char-set 54673 54700) (ucs-range->char-set 54701 54728) (ucs-range->char-set 54729 54756) (ucs-range->char-set 54757 54784) (ucs-range->char-set 54785 54812) (ucs-range->char-set 54813 54840) (ucs-range->char-set 54841 54868) (ucs-range->char-set 54869 54896) (ucs-range->char-set 54897 54924) (ucs-range->char-set 54925 54952) (ucs-range->char-set 54953 54980) (ucs-range->char-set 54981 55008) (ucs-range->char-set 55009 55036) (ucs-range->char-set 55037 55064) (ucs-range->char-set 55065 55092) (ucs-range->char-set 55093 55120) (ucs-range->char-set 55121 55148) (ucs-range->char-set 55149 55176) (ucs-range->char-set 55177 55204))))


;;;; string cursors

(define string-cursor? integer?)

(define (string-start-arg s o)
  (if (pair? o) (string-index->offset s (car o)) 0))
(define (string-end-arg s o)
  (if (pair? o) (string-index->offset s (car o)) (string-length s)))
(define string-cursor=? =)
(define string-cursor<? <)
(define string-cursor<=? <=)
(define string-cursor>? >)
(define string-cursor>=? >=)
(define string-cursor-ref string-ref)
(define (string-cursor-next s i) (+ i 1))
(define (string-cursor-prev s i) (- i 1))
(define substring-cursor substring)
(define (string-offset->index str off) off)
(define (string-index->offset str i) i)
;;(define (string-concatenate ls) (apply string-append ls))
(define (string-concatenate-reverse ls)
  (string-concatenate (reverse ls)))



;;; An rx represents a start state and meta-info such as the number
;;; and names of submatches.
(define-record-type (Rx make-rx regexp?)
  (fields (mutable start-state rx-start-state rx-start-state-set!)
	  (mutable num-matches rx-num-matches rx-num-matches-set!)
	  (mutable num-save-indexes rx-num-save-indexes rx-num-save-indexes-set!)
	  (mutable match-rules rx-rules rx-rules-set!)
	  (mutable match-names rx-names rx-names-set!)
	  (immutable sre regexp->sre)))

;; Syntactic sugar.
(define-syntax rx
  (syntax-rules ()
    ((rx sre ...)
     (regexp `(: sre ...)))))

;;; A state is a single nfa state with transition rules.
(define-record-type (State %make-state state?)
  (fields
   ;; A boolean indicating if this is an accepting state.
   (mutable accept? state-accept? state-accept?-set!)
   ;; A char or char-set indicating when we can transition.
   ;; Alternately, #f indicates an epsilon transition, while a
   ;; procedure of the form (lambda (ch i matches) ...) is a predicate
   ;; which should return #t if the char matches.
   (mutable chars state-chars state-chars-set!)
   ;; A single integer indicating the match position to record.
   (mutable match state-match state-match-set!)
   ;; The rule for merging ambiguous matches.  Can be any of: left,
   ;; right, (list i j).  Posix semantics are equivalent to using left
   ;; for the beginning of a submatch and right for the end.  List is
   ;; used to capture a list of submatch data in the current match.
   (mutable match-rule state-match-rule state-match-rule-set!)
   ;; The destination if the char match succeeds.
   (mutable next1 state-next1 state-next1-set!)
   ;; An optional additional transition used for forking to two states.
   (mutable next2 state-next2 state-next2-set!)))

(define (make-state accept? chars match match-rule next1 next2)
  (if (and next1 (not (state? next1)))
      (error #f "expected a state" next1))
  (if (and next2 (not (state? next2)))
      (error #f "expected a state" next2))
  (%make-state accept? chars match match-rule next1 next2))

(define ~none 0)
(define ~ci? 1)
(define ~ascii? 2)
(define ~nocapture? 4)

(define (flag-set? flags i) (= i (bitwise-and flags i)))
(define (flag-join a b) (if b (bitwise-ior a b) a))
(define (flag-clear a b) (bitwise-and a (bitwise-not b)))

(define (char-set-ci cset)
  (char-set-fold
   (lambda (ch res)
     (char-set-adjoin! (char-set-adjoin! res (char-upcase ch))
                       (char-downcase ch)))
   (char-set)
   cset))

(define (make-char-state ch flags next)
  (if (flag-set? flags ~ci?)
      (let ((cset (cond ((char? ch) (char-set-ci (char-set ch)))
                        ((char-set? ch) (char-set-ci ch))
                        (else ch))))
        (make-state #f cset #f #f next #f))
      (make-state #f ch #f #f next #f)))
(define (make-fork-state next1 next2)
  (make-state #f #f #f #f next1 next2))
(define (make-epsilon-state next)
  (make-fork-state next #f))
(define (make-accept-state)
  (make-state #t #f #f #f #f #f))

;; A record holding the current match data - essentially a wrapper
;; around a vector, plus a reference to the RX for meta-info.
(define-record-type (Regexp-Match %make-regexp-match regexp-match?)
  (fields (mutable matches regexp-match-matches regexp-match-matches-set!)
	  (immutable rx regexp-match-rx)
	  (immutable string regexp-match-string)))

(define (regexp-match-rules md)
  (rx-rules (regexp-match-rx md)))
(define (regexp-match-names md)
  (rx-names (regexp-match-rx md)))
(define (make-regexp-match len rx str)
  (%make-regexp-match (make-vector len #f) rx str))
(define (make-regexp-match-for-rx rx str)
  (make-regexp-match (rx-num-save-indexes rx) rx str))
(define (regexp-match-count md)
  (- (quotient (vector-length (regexp-match-matches md)) 2) 1))

(define (regexp-match-name-offset md name)
  (let lp ((ls (regexp-match-names md)) (first #f))
    (cond
     ((null? ls) (or first (error #f "unknown match name" md name)))
     ((eq? name (caar ls))
      (if (regexp-match-submatch-start+end md (cdar ls))
          (cdar ls)
          (lp (cdr ls) (or first (cdar ls)))))
     (else (lp (cdr ls) first)))))

(define (regexp-match-ref md n)
  (vector-ref (regexp-match-matches md)
              (if (integer? n)
                  n
                  (regexp-match-name-offset md n))))

(define (regexp-match-set! md n val)
  (vector-set! (regexp-match-matches md) n val))

(define (copy-regexp-match md)
  (let* ((src (regexp-match-matches md))
         (len (vector-length src))
         (dst (make-vector len #f)))
    (do ((i 0 (+ i 1)))
        ((= i len)
         (%make-regexp-match dst (regexp-match-rx md) (regexp-match-string md)))
      (vector-set! dst i (vector-ref src i)))))

;;> Returns the matching result for the given named or indexed
;;> submatch \var{n}, possibly as a list for a submatch-list, or
;;> \scheme{#f} if not matched.

(define (regexp-match-submatch/list md n)
  (let ((n (if (integer? n) n (regexp-match-name-offset md n))))
    (cond
     ((>= n (vector-length (regexp-match-rules md)))
      #f)
     (else
      (let ((rule (vector-ref (regexp-match-rules md) n)))
        (cond
         ((pair? rule)
          (let ((start (regexp-match-ref md (car rule)))
                (end (regexp-match-ref md (cdr rule)))
                (str (regexp-match-string md)))
            (and start end (substring-cursor str start end))))
         (else
          (let ((res (regexp-match-ref md rule)))
            (if (pair? res)
                (reverse res)
                res)))))))))

;;> Returns the matching substring for the given named or indexed
;;> submatch \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch md n)
  (let ((res (regexp-match-submatch/list md n)))
    (if (pair? res) (car res) res)))

(define (regexp-match-submatch-start+end md n)
  (let ((n (if (string-cursor? n) n (regexp-match-name-offset md n))))
    (and (< n (vector-length (regexp-match-rules md)))
         (let ((rule (vector-ref (regexp-match-rules md) n)))
           (if (pair? rule)
               (let ((start (regexp-match-ref md (car rule)))
                     (end (regexp-match-ref md (cdr rule)))
                     (str (regexp-match-string md)))
                 (and start end
                      (cons (string-offset->index str start)
                            (string-offset->index str end))))
               #f)))))

;;> Returns the start index for the given named or indexed submatch
;;> \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch-start md n)
  (cond ((regexp-match-submatch-start+end md n) => car) (else #f)))

;;> Returns the end index for the given named or indexed submatch
;;> \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch-end md n)
  (cond ((regexp-match-submatch-start+end md n) => cdr) (else #f)))

(define (regexp-match-convert recurse? md str)
  (cond
   ((vector? md)
    (let lp ((i 0) (res '()))
      (cond
       ((>= i (vector-length md))
        (reverse res))
       ((string-cursor? (vector-ref md i))
        (lp (+ i 2)
            (cons (substring-cursor str
                                    (vector-ref md i)
                                    (vector-ref md (+ i 1)))
                  res)))
       (else
        (lp (+ i 1)
            (cons (regexp-match-convert recurse? (vector-ref md i) str)
                  res))))))
   ((list? md)
    (if recurse?
        (map (lambda (x) (regexp-match-convert recurse? x str)) (reverse md))
        (regexp-match-convert recurse? (car md) str)))
   ((and (pair? md) (string-cursor? (car md)) (string-cursor? (cdr md)))
    (substring-cursor str (car md) (cdr md)))
   ((regexp-match? md)
    (regexp-match-convert
     recurse? (regexp-match-matches md) (regexp-match-string md)))
   (else
    md)))

;;> Convert an regexp-match result to a list of submatches, beginning
;;> with the full match, using \scheme{#f} for unmatched submatches.

(define (regexp-match->list md)
  (regexp-match-convert #f md #f))

;;> Convert an regexp-match result to a forest of submatches, beginning
;;> with the full match, using \scheme{#f} for unmatched submatches.

(define (regexp-match->sexp md)
  (regexp-match-convert #t md #f))

;; Collect results from a list match.
(define (match-collect md spec)
  (define (match-extract md n)
    (let* ((vec (regexp-match-matches md))
           (rules (regexp-match-rules md))
           (n-rule (vector-ref rules n))
           (rule (vector-ref rules n-rule)))
      (if (pair? rule)
          (let ((start (regexp-match-ref md (car rule)))
                (end (regexp-match-ref md (cdr rule))))
            (and start end (cons start end)))
          (regexp-match-ref md rule))))
  (let ((end (cadr spec))
        (vec (regexp-match-matches md)))
    (let lp ((i (+ 1 (car spec)))
             (ls '()))
      (if (>= i end)
          (reverse ls)
          (lp (+ i 1) (cons (match-extract md i) ls))))))

;; A searcher represents a single rx state and match information.
(define-record-type (Searcher make-searcher searcher?)
  (fields (mutable state searcher-state searcher-state-set!)
	  (mutable matches searcher-matches searcher-matches-set!)))

;; Merge two regexp-matches, preferring the leftmost-longest of their
;; matches.
(define (regexp-match>=? m1 m2)
  (let ((end (- (vector-length (regexp-match-matches m1)) 1)))
    (let lp ((i 0))
      (cond
       ((>= i end)
        #t)
       ((and (eqv? (regexp-match-ref m1 i)
                   (regexp-match-ref m2 i))
             (eqv? (regexp-match-ref m1 (+ i 1))
                   (regexp-match-ref m2 (+ i 1))))
        (lp (+ i 2)))
       ((and (string-cursor? (regexp-match-ref m2 i))
             (or (not (string-cursor? (regexp-match-ref m1 i)))
                 (string-cursor<? (regexp-match-ref m2 i)
                                  (regexp-match-ref m1 i))
                 (and
                  (string-cursor=? (regexp-match-ref m2 i)
                                   (regexp-match-ref m1 i))
                  (or (not (string-cursor? (regexp-match-ref m2 (+ i 1))))
                      (and (string-cursor? (regexp-match-ref m1 (+ i 1)))
                           (string-cursor>? (regexp-match-ref m2 (+ i 1))
                                            (regexp-match-ref m1 (+ i 1))))))))
        #f)
       (else
        #t)))))

(define (regexp-match-max m1 m2)
  (if (regexp-match>=? m1 m2) m1 m2))

;; Merge match data from sr2 into sr1, preferring the leftmost-longest
;; match in the event of a conflict.
(define (searcher-merge! sr1 sr2)
  (let ((m (regexp-match-max (searcher-matches sr1) (searcher-matches sr2))))
    (searcher-matches-set! sr1 m)))

(define (searcher-max sr1 sr2)
  (if (or (not (searcher? sr2))
          (regexp-match>=? (searcher-matches sr1) (searcher-matches sr2)))
      sr1
      sr2))

(define (searcher-start-match sr)
  (regexp-match-ref (searcher-matches sr) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A posse is a group of searchers.

(define (make-posse . o)
  (make-eq-hashtable))

(define posse? hashtable?)
(define (posse-empty? posse) (zero? (hashtable-size posse)))

(define (posse-ref posse sr)
  (hashtable-ref posse (searcher-state sr) #f))
(define (posse-add! posse sr)
  (hashtable-set! posse (searcher-state sr) sr))
(define (posse-clear! posse)
  (hashtable-for-each-entry (lambda (key val) (hashtable-delete! posse key)) posse))
(define (posse-for-each proc posse)
  (hashtable-for-each-entry (lambda (key val) (proc val)) posse))
(define (posse-every pred posse)
  (hashtable-fold-entries
      (lambda (key val acc)
	(and acc (pred val)))
    #t posse))

(define (posse->list posse)
  (receive (keys vals)
      (hashtable-entries posse)
    (vector->list vals)))
(define (list->posse ls)
  (let ((searchers (make-posse)))
    (for-each (lambda (sr) (posse-add! searchers sr)) ls)
    searchers))
(define (posse . args)
  (list->posse args))

(define (make-start-searcher rx str)
  (make-searcher (rx-start-state rx) (make-regexp-match-for-rx rx str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution

;; A transition which doesn't advance the index.

(define (epsilon-state? st)
  (or (not (state-chars st))
      (procedure? (state-chars st))))

;; Match the state against a char and index.

(define (state-matches? st str i ch start end matches)
  (let ((matcher (state-chars st)))
    (cond
     ((char? matcher)
      (eqv? matcher ch))
     ((char-set? matcher)
      (char-set-contains? matcher ch))
     ((pair? matcher)
      (and (char<=? (car matcher) ch) (char<=? ch (cdr matcher))))
     ((procedure? matcher)
      (matcher str i ch start end matches))
     ((not matcher))
     (else
      (error #f "unknown state matcher" (state-chars st))))))

;; Advance epsilons together - if the State is newly added to the
;; group and is an epsilon state, recursively add the transition.

(define (posse-advance! new seen accept sr str i start end)
  (let advance! ((sr sr))
    (let ((st (searcher-state sr)))
      ;; Update match data.
      (cond
       ((state-match st)
        (let ((index (state-match st))
              (matches (searcher-matches sr)))
          (cond
           ((pair? index)
            ;; Submatch list, accumulate and push.
            (let* ((prev (regexp-match-ref matches (car index)))
                   (new (cons (match-collect matches (cdr index))
                              (if (pair? prev) prev '()))))
              (regexp-match-set! matches (car index) new)))
           (else
            (regexp-match-set! matches index i))))))
      ;; Follow transitions.
      (cond
       ((state-accept? st)
        (set-cdr! accept (searcher-max sr (cdr accept))))
       ((posse-ref seen sr)
        => (lambda (sr-prev) (searcher-merge! sr-prev sr)))
       ((epsilon-state? st)
        (let ((ch (and (string-cursor<? i end) (string-cursor-ref str i))))
          ;; Epsilon transition.  If there is a procedure matcher,
          ;; it's a guarded epsilon and needs to be checked.
          (cond
           ((state-matches? st str i ch start end (searcher-matches sr))
            (posse-add! seen sr)
            (let ((next1 (state-next1 st))
                  (next2 (state-next2 st)))
              (cond
               (next1
                (searcher-state-set! sr next1)
                (advance! sr)))
              (cond
               (next2
                (let ((sr2 (make-searcher
                            next2
                            (copy-regexp-match (searcher-matches sr)))))
                  (advance! sr2)))))))))
       ;; Non-special, non-epsilon searcher, add to posse.
       ((posse-ref new sr)
        ;; Merge regexp-match for existing searcher.
        => (lambda (sr-prev) (searcher-merge! sr-prev sr)))
       (else
        ;; Add new searcher.
        (posse-add! new sr))))))

;; Run so long as there is more to match.

(define (regexp-run-offsets search? rx str start end)
  (let ((rx (regexp rx))
        (epsilons (posse))
        (accept (list #f)))
    (let lp ((i start)
             (searchers1 (posse))
             (searchers2 (posse)))
      ;; Advance initial epsilons once from the first index, or every
      ;; time when searching.
      (cond
       ((or search? (string-cursor=? i start))
        (posse-advance! searchers1 epsilons accept (make-start-searcher rx str)
                        str i start end)
        (posse-clear! epsilons)))
      (cond
       ((or (string-cursor>=? i end)
            (and search?
                 (searcher? (cdr accept))
                 (let ((accept-start (searcher-start-match (cdr accept))))
                   (posse-every
                    (lambda (searcher)
                      (> (searcher-start-match searcher) accept-start))
                    searchers1)))
            (and (not search?)
                 (posse-empty? searchers1)))
        ;; Terminate when the string is done or there are no more
        ;; searchers.  If we terminate prematurely and are not
        ;; searching, return false.
        (and (searcher? (cdr accept))
             (let ((matches (searcher-matches (cdr accept))))
               (and (or search? (>= (regexp-match-ref matches 1) end))
                    (searcher-matches (cdr accept))))))
       (else
        ;; Otherwise advance normally.
        (let ((ch (string-cursor-ref str i))
              (i2 (string-cursor-next str i)))
          (posse-for-each  ;; NOTE: non-deterministic from hash order
           (lambda (sr)
             (cond
              ((state-matches? (searcher-state sr) str i ch
                               start end (searcher-matches sr))
               (searcher-state-set! sr (state-next1 (searcher-state sr)))
               ;; Epsilons are considered at the next position.
               (posse-advance! searchers2 epsilons accept sr str i2 start end)
               (posse-clear! epsilons))))
           searchers1)
          (posse-clear! searchers1)
          (lp i2 searchers2 searchers1)))))))

;; Wrapper to determine start and end offsets.

(define (regexp-run search? rx str . o)
  (let ((start (string-start-arg str o))
        (end (string-end-arg str (if (pair? o) (cdr o) o))))
    (regexp-run-offsets search? rx str start end)))

;;> Match the given regexp or SRE against the entire string and return
;;> the match data on success.  Returns \scheme{#f} on failure.

(define (regexp-matches rx str . o)
  (apply regexp-run #f rx str o))

;;> Match the given regexp or SRE against the entire string and return
;;> the \scheme{#t} on success.  Returns \scheme{#f} on failure.

(define (regexp-matches? rx str . o)
  (and (apply regexp-matches rx str o) #t))

;;> Search for the given regexp or SRE within string and return
;;> the match data on success.  Returns \scheme{#f} on failure.

(define (regexp-search rx str . o)
  (apply regexp-run #t rx str o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling

(define (parse-flags ls)
  (define (symbol->flag s)
    (case s ((i ci case-insensitive) ~ci?) (else ~none)))
  (let lp ((ls ls) (res ~none))
    (if (not (pair? ls))
        res
        (lp (cdr ls) (flag-join res (symbol->flag (car ls)))))))

(define char-set:nonl
  (char-set-difference char-set:full (char-set #\newline)))
(define char-set:control (ucs-range->char-set 0 32))
(define char-set:word-constituent
  (char-set-union char-set:letter char-set:digit (char-set #\_)))
(define %char-set:word-constituent
  (char-set-union %char-set:letter %char-set:digit (char-set #\_)))
(define (char-word-constituent? ch)
  (char-set-contains? char-set:word-constituent ch))
(define get-char-set:cased
  (let ((char-set:cased #f))
    (lambda ()
      (if (not char-set:cased)
          (set! char-set:cased
                (char-set-union char-set:upper-case
                                char-set:lower-case
                                char-set:title-case)))
      char-set:cased)))

(define (match/bos str i ch start end matches)
  (string-cursor=? i start))
(define (match/eos str i ch start end matches)
  (string-cursor>=? i end))
(define (match/bol str i ch start end matches)
  (or (string-cursor=? i start)
      (eqv? #\newline (string-cursor-ref str (string-cursor-prev str i)))))
(define (match/eol str i ch start end matches)
  (or (string-cursor>=? i end)
      (eqv? #\newline (string-cursor-ref str i))))
(define (match/bow str i ch start end matches)
  (and (string-cursor<? i end)
       (or (string-cursor=? i start)
           (not (char-word-constituent?
                 (string-cursor-ref str (string-cursor-prev str i)))))
       (char-word-constituent? ch)))
(define (match/eow str i ch start end matches)
  (and (or (string-cursor>=? i end)
           (not (char-word-constituent? ch)))
       (string-cursor>? i start)
       (char-word-constituent?
        (string-cursor-ref str (string-cursor-prev str i)))))
(define (match/nwb str i ch start end matches)
  (and (not (match/bow str i ch start end matches))
       (not (match/eow str i ch start end matches))))
(define (match/bog str i ch start end matches)
  (and (string-cursor<? i end)
       (or (string-cursor=? i start)
           (match/eog str (string-cursor-prev str i) ch start end matches))))
(define (match/eog str i ch start end matches)
  (and (string-cursor>? i start)
       (or (string-cursor>=? i end)
           (let ((m (regexp-search re:grapheme str
                                   (string-offset->index str i)
                                   (string-offset->index str end))))
             (and m (string-cursor<=? (regexp-match-submatch-end m 0) i))))))

(define (lookup-char-set name flags)
  (cond
   ((flag-set? flags ~ascii?)
    (case name
      ((any) char-set:full)
      ((nonl) char-set:nonl)
      ((lower-case lower)
       (if (flag-set? flags ~ci?) %char-set:letter %char-set:lower-case))
      ((upper-case upper)
       (if (flag-set? flags ~ci?) %char-set:letter %char-set:upper-case))
      ((title-case title)
       (if (flag-set? flags ~ci?) %char-set:letter (char-set)))
      ((alphabetic alpha) %char-set:letter)
      ((numeric num digit) %char-set:digit)
      ((alphanumeric alphanum alnum) %char-set:letter+digit)
      ((punctuation punct) %char-set:punctuation)
      ((symbol) %char-set:symbol)
      ((graphic graph) %char-set:graphic)
      ((word-constituent) %char-set:word-constituent)
      ((whitespace white space) %char-set:whitespace)
      ((printing print) %char-set:printing)
      ((control cntrl) %char-set:iso-control)
      ((hex-digit xdigit hex) char-set:hex-digit)
      ((ascii) char-set:ascii)
      (else #f)))
   (else
    (case name
      ((any) char-set:full)
      ((nonl) char-set:nonl)
      ((lower-case lower)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:lower-case))
      ((upper-case upper)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:upper-case))
      ((title-case title)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:title-case))
      ((alphabetic alpha) char-set:letter)
      ((numeric num digit) char-set:digit)
      ((alphanumeric alphanum alnum) char-set:letter+digit)
      ((punctuation punct) char-set:punctuation)
      ((symbol) char-set:symbol)
      ((graphic graph) char-set:graphic)
      ((word-constituent) char-set:word-constituent)
      ((whitespace white space) char-set:whitespace)
      ((printing print) char-set:printing)
      ((control cntrl) char-set:control)
      ((hex-digit xdigit hex) char-set:hex-digit)
      ((ascii) char-set:ascii)
      (else #f)))))

(define (sre-flatten-ranges orig-ls)
  (let lp ((ls orig-ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((string? (car ls))
      (lp (append (string->list (car ls)) (cdr ls)) res))
     ((null? (cdr ls))
      (error #f "unbalanced cset / range" orig-ls))
     ((string? (cadr ls))
      (lp (cons (car ls) (append (string->list (cadr ls)) (cddr ls))) res))
     (else
      (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res))))))

(define (every pred ls)
  (or (null? ls) (and (pred (car ls)) (every pred (cdr ls)))))

(define (char-set-sre? sre)
  (or (char? sre)
      (and (string? sre) (= 1 (string-length sre)))
      (lookup-char-set sre ~none)
      (and (pair? sre)
           (or (string? (car sre))
               (memq (car sre)
                     '(char-set / char-range & and ~ complement - difference))
               (and (memq (car sre)
			  ;;;(|\|| or w/case w/nocase w/unicode w/ascii)
                          '(or w/case w/nocase w/unicode w/ascii))
                    (every char-set-sre? (cdr sre)))))))

(define (valid-sre? x)
  (guard (exn (else #f)) (regexp x) #t))

(define (sre->char-set sre . o)
  (let ((flags (if (pair? o) (car o) ~none)))
    (define (->cs sre) (sre->char-set sre flags))
    (define (maybe-ci sre)
      (if (flag-set? flags ~ci?) (char-set-ci sre) sre))
    (cond
     ((lookup-char-set sre flags))
     ((char-set? sre) (maybe-ci sre))
     ((char? sre) (maybe-ci (char-set sre)))
     ((string? sre)
      (if (= 1 (string-length sre))
          (maybe-ci (string->char-set sre))
          (error #f "only single char strings can be char-sets")))
     ((pair? sre)
      (if (string? (car sre))
          (maybe-ci (string->char-set (car sre)))
          (case (car sre)
            ((char-set) (maybe-ci (string->char-set (cadr sre))))
            ((/ char-range)
             (->cs
              `(or ,@(map (lambda (x)
                            (ucs-range->char-set
                             (char->integer (car x))
                             (+ 1 (char->integer (cdr x)))))
                          (sre-flatten-ranges (cdr sre))))))
            ((& and) (apply char-set-intersection (map ->cs (cdr sre))))
            ;;;((|\|| or) (apply char-set-union (map ->cs (cdr sre))))
            ((or) (apply char-set-union (map ->cs (cdr sre))))
            ((~ complement) (char-set-complement (->cs `(or ,@(cdr sre)))))
            ((- difference) (char-set-difference (->cs (cadr sre))
                                                 (->cs `(or ,@(cddr sre)))))
            ((w/case) (sre->char-set (cadr sre) (flag-clear flags ~ci?)))
            ((w/nocase) (sre->char-set (cadr sre) (flag-join flags ~ci?)))
            ((w/ascii) (sre->char-set (cadr sre) (flag-join flags ~ascii?)))
            ((w/unicode) (sre->char-set (cadr sre) (flag-clear flags ~ascii?)))
            (else (error #f "invalid sre char-set" sre)))))
     (else (error #f "invalid sre char-set" sre)))))

(define (char-set->sre cset)
  (list (char-set->string cset)))

(define (strip-submatches sre)
  (if (pair? sre)
      (case (car sre)
        (($ submatch) (strip-submatches (cons ': (cdr sre))))
        ((-> => submatch-named) (strip-submatches (cons ': (cddr sre))))
        (else (cons (strip-submatches (car sre))
                    (strip-submatches (cdr sre)))))
      sre))

(define (sre-expand-reps from to sre)
  (let ((sre0 (strip-submatches sre)))
    (let lp ((i 0) (res '(:)))
      (if (= i from)
          (cond
           ((not to)
            (reverse (cons `(* ,sre) res)))
           ((= from to)
            (reverse (cons sre (cdr res))))
           (else
            (let lp ((i (+ i 1)) (res res))
              (if (>= i to)
                  (reverse (cons `(? ,sre) res))
                  (lp (+ i 1) (cons `(? ,sre0) res))))))
          (lp (+ i 1) (cons sre0 res))))))

;;> Compile an \var{sre} into a regexp.

(define (regexp sre . o)
  (define current-index 2)
  (define current-match 0)
  (define match-names '())
  (define match-rules (list (cons 0 1)))
  (define (make-submatch-state sre flags next index)
    (let* ((n3 (make-epsilon-state next))
           (n2 (->rx sre flags n3))
           (n1 (make-epsilon-state n2)))
      (state-match-set! n1 index)
      (state-match-rule-set! n1 'left)
      (state-match-set! n3 (+ index 1))
      (state-match-rule-set! n3 'right)
      n1))
  (define (->rx sre flags next)
    (cond
     ;; The base cases chars and strings match literally.
     ((char? sre)
      (make-char-state sre flags next))
     ((char-set? sre)
      (make-char-state sre flags next))
     ((string? sre)
      (->rx (cons 'seq (string->list sre)) flags next))
     ((and (symbol? sre) (lookup-char-set sre flags))
      => (lambda (cset) (make-char-state cset ~none next)))
     ((symbol? sre)
      (case sre
        ((epsilon) next)
        ((bos) (make-char-state match/bos flags next))
        ((eos) (make-char-state match/eos flags next))
        ((bol) (make-char-state match/bol flags next))
        ((eol) (make-char-state match/eol flags next))
        ((bow) (make-char-state match/bow flags next))
        ((eow) (make-char-state match/eow flags next))
        ((nwb) (make-char-state match/nwb flags next))
        ((bog) (make-char-state match/bog flags next))
        ((eog) (make-char-state match/eog flags next))
        ((grapheme)
         (->rx
          `(or (: (* ,char-set:hangul-l) (+ ,char-set:hangul-v)
                  (* ,char-set:hangul-t))
               (: (* ,char-set:hangul-l) ,char-set:hangul-v
                  (* ,char-set:hangul-v) (* ,char-set:hangul-t))
               (: (* ,char-set:hangul-l) ,char-set:hangul-lvt
                  (* ,char-set:hangul-t))
               (+ ,char-set:hangul-l)
               (+ ,char-set:hangul-t)
               (+ ,char-set:regional-indicator)
               (: "\r\n")
               (: (~ control ("\r\n"))
                  (+ ,char-set:extend-or-spacing-mark))
               control)
          flags
          next))
        ((word) (->rx '(word+ any) flags next))
        (else (error #f "unknown sre" sre))))
     ((pair? sre)
      (case (car sre)
        ((seq :)
         ;; Sequencing.  An empty sequence jumps directly to next,
         ;; otherwise we join the first element to the sequence formed
         ;; of the remaining elements followed by next.
         (if (null? (cdr sre))
             next
             ;; Make a dummy intermediate to join the states so that
             ;; we can generate n1 first, preserving the submatch order.
             (let* ((n2 (make-epsilon-state #f))
                    (n1 (->rx (cadr sre) flags n2))
                    (n3 (->rx (cons 'seq (cddr sre)) flags next)))
               (state-next1-set! n2 n3)
               n1)))
        ;;;((or |\||)
        ((or)
         ;; Alternation.  An empty alternation always fails.
         ;; Otherwise we fork between any of the alternations, each
         ;; continuing to next.
         (cond
          ((null? (cdr sre))
           #f)
          ((char-set-sre? sre)
           (make-char-state (sre->char-set sre) flags next))
          ((null? (cddr sre))
           (->rx (cadr sre) flags next))
          (else
           (let* ((n1 (->rx (cadr sre) flags next))
                  (n2 (->rx (cons 'or (cddr sre)) flags next)))
             (make-fork-state n1 n2)))))
        ((? optional)
         ;; Optionality.  Either match the body or fork to the next
         ;; state directly.
         (make-fork-state (->rx (cons 'seq (cdr sre)) flags next) next))
        ((* zero-or-more)
         ;; Repetition.  Introduce two fork states which can jump from
         ;; the end of the loop to the beginning and from the
         ;; beginning to the end (to skip the first iteration).
         (let* ((n2 (make-fork-state next #f))
                (n1 (make-fork-state (->rx (cons 'seq (cdr sre)) flags n2) n2)))
           (state-next2-set! n2 n1)
           n1))
        ((+ one-or-more)
         ;; One-or-more repetition.  Same as above but the first
         ;; transition is required so the rx is simpler - we only
         ;; need one fork from the end of the loop to the beginning.
         (let* ((n2 (make-fork-state next #f))
                (n1 (->rx (cons 'seq (cdr sre)) flags n2)))
           (state-next2-set! n2 n1)
           n1))
        ((= exactly)
         ;; Exact repetition.
         (->rx (sre-expand-reps (cadr sre) (cadr sre) (cons 'seq (cddr sre)))
               flags next))
        ((>= at-least)
         ;; n-or-more repetition.
         (->rx (sre-expand-reps (cadr sre) #f (cons 'seq (cddr sre)))
               flags next))
        ((** repeated)
         ;; n-to-m repetition.
         (->rx (sre-expand-reps (cadr sre) (car (cddr sre))
                                (cons 'seq (cdr (cddr sre))))
               flags next))
        ((-> => submatch-named)
         ;; Named submatches just record the name for the current
         ;; match and rewrite as a non-named submatch.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cddr sre)) flags next))
          (else
           (set! match-names
                 (cons (cons (cadr sre) (+ 1 current-match)) match-names))
           (->rx (cons 'submatch (cddr sre)) flags next))))
        ((*-> *=> submatch-named-list)
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cddr sre)) flags next))
          (else
           (set! match-names (cons (cons (cadr sre) current-match) match-names))
           (->rx (cons 'submatch-list (cddr sre)) flags next))))
        (($ submatch)
         ;; A submatch wraps next with an epsilon transition before
         ;; next, setting the start and end index on the result and
         ;; wrapped next respectively.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cdr sre)) flags next))
          (else
           (let ((num current-match)
                 (index current-index))
             (set! current-match (+ current-match 1))
             (set! current-index (+ current-index 2))
             (set! match-rules `((,index . ,(+ index 1)) ,@match-rules))
             (make-submatch-state (cons 'seq (cdr sre)) flags next index)))))
        ((*$ submatch-list)
         ;; A submatch-list wraps a range of submatch results into a
         ;; single match value.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cdr sre)) flags next))
          (else
           (let* ((num current-match)
                  (index current-index))
             (set! current-match (+ current-match 1))
             (set! current-index (+ current-index 1))
             (set! match-rules `(,index ,@match-rules))
             (let* ((n2 (make-epsilon-state next))
                    (n1 (->rx (cons 'submatch (cdr sre)) flags n2)))
               (state-match-set! n2 (list index num current-match))
               (state-match-rule-set! n2 'list)
               n1)))))
        ((~ - & / complement difference and char-range char-set)
         (make-char-state (sre->char-set sre flags) ~none next))
        ((word)
         (->rx `(: bow ,@(cdr sre) eow) flags next))
        ((word+)
         (->rx `(word (+ ,(if (equal? '(any) (cdr sre))
                              'word-constituent
                              (char-set-intersection
                               char-set:word-constituent
                               (sre->char-set `(or ,@(cdr sre)) flags)))))
               flags
               next))
        ((w/case)
         (->rx `(: ,@(cdr sre)) (flag-clear flags ~ci?) next))
        ((w/nocase)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~ci?) next))
        ((w/unicode)
         (->rx `(: ,@(cdr sre)) (flag-clear flags ~ascii?) next))
        ((w/ascii)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~ascii?) next))
        ((w/nocapture)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~nocapture?) next))
        (else
         (if (string? (car sre))
             (make-char-state (sre->char-set sre flags) ~none next)
             (error #f "unknown sre" sre)))))))
  (let ((flags (parse-flags (and (pair? o) (car o)))))
    (if (regexp? sre)
        sre
        (let ((start (make-submatch-state sre flags (make-accept-state) 0)))
          (make-rx start current-match current-index
                   (list->vector (reverse match-rules)) match-names sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;;> The fundamental regexp matching iterator.  Repeatedly searches
;;> \var{str} for the regexp \var{re} so long as a match can be found.
;;> On each successful match, applies \scheme{(\var{kons} \var{i}
;;> \var{regexp-match} \var{str} \var{acc})} where \var{i} is the
;;> index since the last match (beginning with
;;> \var{start}),\var{regexp-match} is the resulting match, and
;;> \var{acc} is the result of the previous \var{kons} application,
;;> beginning with \var{knil}.  When no more matches can be found,
;;> calls \var{finish} with the same arguments, except that
;;> \var{regexp-match} is \scheme{#f}.
;;>
;;> By default \var{finish} just returns \var{acc}.

(define (regexp-fold rx kons knil str . o)
  (let* ((rx (regexp rx))
         (finish (if (pair? o) (car o) (lambda (from md str acc) acc)))
         (o (if (pair? o) (cdr o) o))
         (start (string-start-arg str o))
         (end (string-end-arg str (if (pair? o) (cdr o) o))))
    (let lp ((i start)
             (from start)
             (acc knil))
      (cond
       ((and (string-cursor<? i end) (regexp-run-offsets #t rx str i end))
        => (lambda (md)
             (let ((j (regexp-match-ref md 1)))
               (lp (if (and (string-cursor=? i j) (string-cursor<? j end))
                       (string-cursor-next str j)
                       j)
                   j
                   (kons (string-offset->index str from) md str acc)))))
       (else
        (finish (string-offset->index str from) #f str acc))))))

;;> Extracts all non-empty substrings of \var{str} which match
;;> \var{re} between \var{start} and \var{end} as a list of strings.

(define (regexp-extract rx str . o)
  (apply regexp-fold
         rx
         (lambda (from md str a)
           (let ((s (regexp-match-submatch md 0)))
             (if (equal? s "") a (cons s a))))
         '()
         str
         (lambda (from md str a) (reverse a))
         o))

;;> Splits \var{str} into a list of strings separated by matches of
;;> \var{re}.

(define (regexp-split rx str . o)
  ;; start and end in indices passed to regexp-fold
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (regexp-fold
     rx
     (lambda (from md str a)
       (let ((i (regexp-match-submatch-start md 0)))
         (if (< from i) (cons (substring str from i) a) a)))
     '()
     str
     (lambda (from md str a)
       (reverse (if (< from end) (cons (substring str from end) a) a)))
     start
     end)))

;;> Partitions \var{str} into a list of non-empty strings
;;> matching \var{re}, interspersed with the unmatched portions
;;> of the string.  The first and every odd element is an unmatched
;;> substring, which will be the empty string if \var{re} matches
;;> at the beginning of the string or end of the previous match.  The
;;> second and every even element will be a substring matching
;;> \var{re}.  If the final match ends at the end of the string,
;;> no trailing empty string will be included.  Thus, in the
;;> degenerate case where \var{str} is the empty string, the
;;> result is \scheme{("")}.

(define (regexp-partition rx str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (define (kons from md str a)
      (let ((left (substring str from (regexp-match-submatch-start md 0))))
        (cons (regexp-match-submatch md 0) (cons left a))))
    (define (final from md str a)
      (if (or (< from end) (null? a))
          (cons (substring str from end) a)
          a))
    (reverse (regexp-fold rx kons '() str final start end))))

;;> Returns a new string replacing the \var{count}th match of \var{re}
;;> in \var{str} the \var{subst}, where the zero-indexed \var{count}
;;> defaults to zero (i.e. the first match).  If there are not
;;> \var{count} matches, returns the selected substring unmodified.

;;> \var{subst} can be a string, an integer or symbol indicating the
;;> contents of a numbered or named submatch of \var{re},\scheme{'pre}
;;> for the substring to the left of the match, or \scheme{'post} for
;;> the substring to the right of the match.

;;> The optional parameters \var{start} and \var{end} restrict both
;;> the matching and the substitution, to the given indices, such that
;;> the result is equivalent to omitting these parameters and
;;> replacing on \scheme{(substring str start end)}. As a convenience,
;;> a value of \scheme{#f} for \var{end} is equivalent to
;;> \scheme{(string-length str)}.

(define (regexp-replace rx str subst . o)
  (let* ((start (if (and (pair? o) (car o)) (car o) 0))
         (o (if (pair? o) (cdr o) '()))
         (end (if (and (pair? o) (car o)) (car o) (string-length str)))
         (o (if (pair? o) (cdr o) '()))
         (count (if (pair? o) (car o) 0)))
    (let lp ((i start) (count count))
      (let ((m (regexp-search rx str i end)))
        (cond
         ((not m) str)
         ((positive? count)
          (lp (regexp-match-submatch-end m 0) (- count 1)))
         (else
          (string-concatenate
           (cons
            (substring str start (regexp-match-submatch-start m 0))
            (append
             (reverse (regexp-apply-match m str subst))
             (list (substring str (regexp-match-submatch-end m 0) end)))))))))))

;;> Equivalent to \var{regexp-replace}, but replaces all occurrences
;;> of \var{re} in \var{str}.

(define (regexp-replace-all rx str subst . o)
  (regexp-fold
   rx
   (lambda (i m str acc)
     (let ((m-start (regexp-match-submatch-start m 0)))
       (append (regexp-apply-match m str subst)
               (if (>= i m-start)
                   acc
                   (cons (substring str i m-start) acc)))))
   '()
   str
   (lambda (i m str acc)
     (let ((end (string-length str)))
       (string-concatenate-reverse
        (if (>= i end)
            acc
            (cons (substring str i end) acc)))))))

(define (regexp-apply-match m str ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      res)
     ((not (pair? ls))
      (lp (list ls) res))
     ((integer? (car ls))
      (lp (cdr ls) (cons (or (regexp-match-submatch m (car ls)) "") res)))
     ((procedure? (car ls))
      (lp (cdr ls) (cons ((car ls) m) res)))
     ((symbol? (car ls))
      (case (car ls)
        ((pre)
         (lp (cdr ls)
             (cons (substring-cursor str 0 (regexp-match-submatch-start m 0))
                   res)))
        ((post)
         (lp (cdr ls)
             (cons (substring str
                              (regexp-match-submatch-end m 0)
                              (string-length str))
                   res)))
        (else
         (cond
          ((assq (car ls) (regexp-match-names m))
           => (lambda (x) (lp (cons (cdr x) (cdr ls)) res)))
          (else
           (error #f "unknown match replacement" (car ls)))))))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define re:grapheme (regexp 'grapheme))


;;;; done

#| end of library |# )

;;; end of file
