;;; -*- mode: scheme; coding: utf-8-unix; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex macro definitions to parse R6RS identifiers
;;;Date: Tue Jan 18, 2011
;;;
;;;Abstract
;;;
;;;	This file  needs the macro file "unicode.macros.l"  to be loaded
;;;	first.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;page
UNICODE-Nd-Mc-Me	{UNICODE-Nd}|{UNICODE-Mc}|{UNICODE-Me}

;;Any  character whose  Unicode scalar  value is  greater than  127, and
;;whose category is Co, Ll, Lm, Lo,  Lt, Lu, Mn, Nl, No, Pc, Pd, Po, Sc,
;;Sk, Sm or So.
;;
;;The  character range  below has  been constructed  with  the following
;;script:
;; #!r6rs
;; (import (nausicaa)
;;   (nausicaa char-sets)
;;   (nausicaa char-sets categories)
;;   (nausicaa formations))
;;
;; (define identifier-cs
;;  (char-set-complement
;;   (char-set `(,(integer->char 0) . ,(integer->char 127)))
;;   (char-set-union char-set:category/Co char-set:category/Lu
;; 		  char-set:category/Ll char-set:category/Lt
;; 		  char-set:category/Lm char-set:category/Lo
;; 		  char-set:category/Mn char-set:category/Nl
;; 		  char-set:category/No char-set:category/Pc
;; 		  char-set:category/Pd char-set:category/Po
;; 		  char-set:category/Sc char-set:category/Sk
;; 		  char-set:category/Sm char-set:category/So)))
;;
;; (display "[")
;; (for-each (lambda (pair)
;; 	    (format #t "#x~a-#x~a"
;; 		    (number->string (char->integer (car pair)) 16)
;; 		    (number->string (char->integer (cdr pair)))))
;;   (char-set-domain-ref identifier-cs))
;; (display "]\n")
;;
;;CONSTITUENT_TAIL	[\127-\55295]|[\57344-\1114111]
CONSTITUENT_TAIL	[#xA1-#x170#xAC-#x172#xAE-#x186#xBC-#x887#x37A-#x894#x384-#x906#x38C-#x908#x38E-#x929#x3A3-#x1159#x48A-#x1315#x531-#x1366#x559-#x1375#x561-#x1415#x589-#x1418#x591-#x1479#x5D0-#x1514#x5F0-#x1524#x606-#x1563#x61E-#x1567#x621-#x1630#x66A-#x1756#x6DF-#x1775#x6FA-#x1805#x710-#x1866#x74D-#x1969#x7CA-#x2042#x901-#x2306#x904-#x2361#x93C-#x2365#x941-#x2376#x94D-#x2381#x950-#x2388#x958-#x2405#x970-#x2418#x97B-#x2431#x981-#x2433#x985-#x2444#x98F-#x2448#x993-#x2472#x9AA-#x2480#x9B2-#x2482#x9B6-#x2489#x9BC-#x2493#x9C1-#x2500#x9CD-#x2510#x9DC-#x2525#x9DF-#x2531#x9F0-#x2554#xA01-#x2562#xA05-#x2570#xA0F-#x2576#xA13-#x2600#xA2A-#x2608#xA32-#x2611#xA35-#x2614#xA38-#x2617#xA3C-#x2620#xA41-#x2626#xA47-#x2632#xA4B-#x2637#xA51-#x2641#xA59-#x2652#xA5E-#x2654#xA70-#x2677#xA81-#x2690#xA85-#x2701#xA8F-#x2705#xA93-#x2728#xAAA-#x2736#xAB2-#x2739#xAB5-#x2745#xABC-#x2749#xAC1-#x2757#xAC7-#x2760#xACD-#x2765#xAD0-#x2768#xAE0-#x2787#xAF1-#x2801#xB01-#x2817#xB05-#x2828#xB0F-#x2832#xB13-#x2856#xB2A-#x2864#xB32-#x2867#xB35-#x2873#xB3C-#x2877#xB3F-#x2879#xB41-#x2884#xB4D-#x2893#xB56-#x2902#xB5C-#x2909#xB5F-#x2915#xB70-#x2929#xB82-#x2947#xB85-#x2954#xB8E-#x2960#xB92-#x2965#xB99-#x2970#xB9C-#x2972#xB9E-#x2975#xBA3-#x2980#xBA8-#x2986#xBAE-#x3001#xBC0-#x3008#xBCD-#x3021#xBD0-#x3024#xBF0-#x3066#xC05-#x3084#xC0E-#x3088#xC12-#x3112#xC2A-#x3123#xC35-#x3129#xC3D-#x3136#xC46-#x3144#xC4A-#x3149#xC55-#x3158#xC58-#x3161#xC60-#x3171#xC78-#x3199#xC85-#x3212#xC8E-#x3216#xC92-#x3240#xCAA-#x3251#xCB5-#x3257#xCBC-#x3261#xCBF-#x3263#xCC6-#x3270#xCCC-#x3277#xCDE-#x3294#xCE0-#x3299#xCF1-#x3314#xD05-#x3340#xD0E-#x3344#xD12-#x3368#xD2A-#x3385#xD3D-#x3389#xD41-#x3396#xD4D-#x3405#xD60-#x3427#xD70-#x3445#xD79-#x3455#xD85-#x3478#xD9A-#x3505#xDB3-#x3515#xDBD-#x3517#xDC0-#x3526#xDCA-#x3530#xDD2-#x3540#xDD6-#x3542#xDF4-#x3572#xE01-#x3642#xE3F-#x3663#xE5A-#x3675#xE81-#x3714#xE84-#x3716#xE87-#x3720#xE8A-#x3722#xE8D-#x3725#xE94-#x3735#xE99-#x3743#xEA1-#x3747#xEA5-#x3749#xEA7-#x3751#xEAA-#x3755#xEAD-#x3769#xEBB-#x3773#xEC0-#x3780#xEC6-#x3782#xEC8-#x3789#xEDC-#x3805#xF00-#x3871#xF2A-#x3897#xF40-#x3911#xF49-#x3948#xF71-#x3966#xF80-#x3979#xF90-#x3991#xF99-#x4028#xFBE-#x4044#xFCE-#x4052#x1000-#x4138#x102D-#x4144#x1032-#x4151#x1039-#x4154#x103D-#x4159#x104A-#x4181#x1058-#x4193#x1065-#x4198#x106E-#x4226#x1085-#x4230#x108D-#x4238#x109E-#x4293#x10D0-#x4348#x1100-#x4441#x115F-#x4514#x11A8-#x4601#x1200-#x4680#x124A-#x4685#x1250-#x4694#x1258-#x4696#x125A-#x4701#x1260-#x4744#x128A-#x4749#x1290-#x4784#x12B2-#x4789#x12B8-#x4798#x12C0-#x4800#x12C2-#x4805#x12C8-#x4822#x12D8-#x4880#x1312-#x4885#x1318-#x4954#x135F-#x4988#x1380-#x5017#x13A0-#x5108#x1401-#x5750#x1681-#x5786#x16A0-#x5872#x1700-#x5900#x170E-#x5908#x1720-#x5942#x1740-#x5971#x1760-#x5996#x176E-#x6000#x1772-#x6003#x1780-#x6067#x17B7-#x6077#x17C6-#x6086#x17C9-#x6109#x17F0-#x6137#x1800-#x6157#x1820-#x6263#x1880-#x6314#x1900-#x6428#x1920-#x6434#x1927-#x6440#x1932-#x6450#x1939-#x6459#x1940-#x6464#x1944-#x6469#x1950-#x6509#x1970-#x6516#x1980-#x6569#x19C1-#x6599#x19DE-#x6680#x1A1E-#x6687#x1B00-#x6915#x1B05-#x6964#x1B36-#x6970#x1B3C-#x6972#x1B42-#x6978#x1B45-#x6987#x1B5A-#x7036#x1B80-#x7041#x1B83-#x7072#x1BA2-#x7077#x1BA8-#x7081#x1BAE-#x7087#x1C00-#x7203#x1C2C-#x7219#x1C36-#x7223#x1C3B-#x7231#x1C4D-#x7247#x1C5A-#x7295#x1D00-#x7654#x1DFE-#x7957#x1F18-#x7965#x1F20-#x8005#x1F48-#x8013#x1F50-#x8023#x1F59-#x8025#x1F5B-#x8027#x1F5D-#x8029#x1F5F-#x8061#x1F80-#x8116#x1FB6-#x8132#x1FC6-#x8147#x1FD6-#x8155#x1FDD-#x8175#x1FF2-#x8180#x1FF6-#x8190#x2010-#x8215#x2020-#x8231#x2030-#x8248#x203B-#x8260#x2047-#x8286#x2070-#x8305#x2074-#x8316#x207F-#x8332#x2090-#x8340#x20A0-#x8373#x20D0-#x8412#x20E1-#x8417#x20E5-#x8432#x2100-#x8527#x2153-#x8584#x2190-#x9000#x232B-#x9191#x2400-#x9254#x2440-#x9290#x2460-#x9885#x26A0-#x9916#x26C0-#x9923#x2701-#x9988#x2706-#x9993#x270C-#x10023#x2729-#x10059#x274D-#x10061#x274F-#x10066#x2756-#x10070#x2758-#x10078#x2761-#x10087#x2776-#x10132#x2798-#x10159#x27B1-#x10174#x27C0-#x10180#x27C7-#x10186#x27CC-#x10188#x27D0-#x10213#x27F0-#x10626#x2999-#x10711#x29DC-#x10747#x29FE-#x11084#x2B50-#x11092#x2C00-#x11310#x2C30-#x11358#x2C60-#x11375#x2C71-#x11389#x2C80-#x11498#x2CF9-#x11557#x2D30-#x11621#x2D6F-#x11631#x2D80-#x11670#x2DA0-#x11686#x2DA8-#x11694#x2DB0-#x11702#x2DB8-#x11710#x2DC0-#x11718#x2DC8-#x11726#x2DD0-#x11734#x2DD8-#x11742#x2DE0-#x11777#x2E06-#x11784#x2E0B-#x11787#x2E0E-#x11803#x2E1E-#x11807#x2E2A-#x11824#x2E80-#x11929#x2E9B-#x12019#x2F00-#x12245#x2FF0-#x12283#x3001-#x12295#x3012-#x12307#x301C-#x12316#x3020-#x12351#x3041-#x12438#x3099-#x12543#x3105-#x12589#x3131-#x12686#x3190-#x12727#x31C0-#x12771#x31F0-#x12830#x3220-#x12867#x3250-#x13054#x3300-#x19893#x4DC0-#x40899#xA000-#x42124#xA490-#x42182#xA500-#x42527#xA62A-#x42539#xA640-#x42591#xA662-#x42607#xA673-#x42611#xA67C-#x42647#xA700-#x42892#xA7FB-#x43042#xA825-#x43046#xA828-#x43051#xA840-#x43127#xA882-#x43187#xA8C4-#x43204#xA8CE-#x43215#xA90A-#x43345#xA95F-#x43359#xAA00-#x43566#xAA31-#x43570#xAA35-#x43574#xAA40-#x43596#xAA5C-#x43615#xAC00-#x55203#xE000-#x64045#xFA30-#x64106#xFA70-#x64217#xFB00-#x64262#xFB13-#x64279#xFB1D-#x64310#xFB38-#x64316#xFB3E-#x64318#xFB40-#x64321#xFB43-#x64324#xFB46-#x64433#xFBD3-#x64829#xFD50-#x64911#xFD92-#x64967#xFDF0-#x65021#xFE00-#x65046#xFE19-#x65049#xFE20-#x65062#xFE30-#x65076#xFE45-#x65094#xFE49-#x65106#xFE54-#x65112#xFE5F-#x65126#xFE68-#x65131#xFE70-#x65140#xFE76-#x65276#xFF01-#x65287#xFF0A-#x65295#xFF1A-#x65338#xFF3C-#x65340#xFF3E-#x65370#xFF5C-#x65372#xFF5E-#x65374#xFF61-#x65377#xFF64-#x65470#xFFC2-#x65479#xFFCA-#x65487#xFFD2-#x65495#xFFDA-#x65500#xFFE0-#x65510#xFFE8-#x65518#xFFFC-#x65533#x10000-#x65547#x1000D-#x65574#x10028-#x65594#x1003C-#x65597#x1003F-#x65613#x10050-#x65629#x10080-#x65786#x10100-#x65794#x10107-#x65843#x10137-#x65930#x10190-#x65947#x101D0-#x66045#x10280-#x66204#x102A0-#x66256#x10300-#x66334#x10320-#x66339#x10330-#x66378#x10380-#x66461#x1039F-#x66499#x103C8-#x66517#x10400-#x66717#x10800-#x67589#x10808-#x67592#x1080A-#x67637#x10837-#x67640#x1083C-#x67644#x1083F-#x67647#x10900-#x67865#x1091F-#x67897#x1093F-#x67903#x10A00-#x68099#x10A05-#x68102#x10A0C-#x68115#x10A15-#x68119#x10A19-#x68147#x10A38-#x68154#x10A3F-#x68167#x10A50-#x68184#x12000-#x74606#x12400-#x74850#x12470-#x74867#x1D000-#x119029#x1D100-#x119078#x1D129-#x119140#x1D167-#x119148#x1D17B-#x119261#x1D200-#x119365#x1D300-#x119638#x1D360-#x119665#x1D400-#x119892#x1D456-#x119964#x1D49E-#x119967#x1D4A2-#x119970#x1D4A5-#x119974#x1D4A9-#x119980#x1D4AE-#x119993#x1D4BB-#x119995#x1D4BD-#x120003#x1D4C5-#x120069#x1D507-#x120074#x1D50D-#x120084#x1D516-#x120092#x1D51E-#x120121#x1D53B-#x120126#x1D540-#x120132#x1D546-#x120134#x1D54A-#x120144#x1D552-#x120485#x1D6A8-#x120779#x1F000-#x127019#x1F030-#x127123#x20000-#x173782#x2F800-#x195101#xE0100-#x917999#xF0000-#x1048573#x100000-#x1114109]

;;page
LETTER			[a-zA-Z]
CONSTITUENT		{LETTER}|{CONSTITUENT_TAIL}
SPECIAL_INITIAL		[!\$%&\*/:<=>\?\^_~]
DIGIT			[0-9]
HEXDIGIT		{DIGIT}|[a-fA-F]
SPECIAL_SUBSEQUENT	[\+\-\.@]
HEX_SCALAR_VALUE	{HEXDIGIT}+
INLINE_HEX_ESCAPE	"\\x"{HEX_SCALAR_VALUE}";"
INITIAL			{CONSTITUENT}|{SPECIAL_INITIAL}|{INLINE_HEX_ESCAPE}
SUBSEQUENT		{INITIAL}|{DIGIT}|{UNICODE-Nd-Mc-Me}|{SPECIAL_SUBSEQUENT}
IDENTIFIER		{INITIAL}{SUBSEQUENT}*
PECULIAR_IDENTIFIER	[\+\-]|"..."|(\->{SUBSEQUENT}*)

;;; end of file
