;;;
;;;Part of: Vicare Scheme
;;;Contents: character sets from "Blocks.txt"
;;;Date: Tue Jun 23, 2009
;;;
;;;Abstract
;;;
;;;	This file holds  one character set definition for  each range of
;;;	code points  defined in the  file "Blocks.txt" from  the Unicode
;;;	Characters Database:
;;;
;;;		<ftp://ftp.unicode.org/Public/UNIDATA/Blocks.txt>
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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



#!r6rs
(library (vicare containers char-sets blocks)
  (export
    char-set:block/basic-latin
    char-set:block/latin-1-supplement
    char-set:block/latin-extended-a
    char-set:block/latin-extended-b
    char-set:block/ipa-extensions
    char-set:block/spacing-modifier-letters
    char-set:block/combining-diacritical-marks
    char-set:block/greek-and-coptic
    char-set:block/cyrillic
    char-set:block/cyrillic-supplement
    char-set:block/armenian
    char-set:block/hebrew
    char-set:block/arabic
    char-set:block/syriac
    char-set:block/arabic-supplement
    char-set:block/thaana
    char-set:block/nko
    char-set:block/devanagari
    char-set:block/bengali
    char-set:block/gurmukhi
    char-set:block/gujarati
    char-set:block/oriya
    char-set:block/tamil
    char-set:block/telugu
    char-set:block/kannada
    char-set:block/malayalam
    char-set:block/sinhala
    char-set:block/thai
    char-set:block/lao
    char-set:block/tibetan
    char-set:block/myanmar
    char-set:block/georgian
    char-set:block/hangul-jamo
    char-set:block/ethiopic
    char-set:block/ethiopic-supplement
    char-set:block/cherokee
    char-set:block/unified-canadian-aboriginal-syllabics
    char-set:block/ogham
    char-set:block/runic
    char-set:block/tagalog
    char-set:block/hanunoo
    char-set:block/buhid
    char-set:block/tagbanwa
    char-set:block/khmer
    char-set:block/mongolian
    char-set:block/limbu
    char-set:block/tai-le
    char-set:block/new-tai-lue
    char-set:block/khmer-symbols
    char-set:block/buginese
    char-set:block/balinese
    char-set:block/sundanese
    char-set:block/lepcha
    char-set:block/ol-chiki
    char-set:block/phonetic-extensions
    char-set:block/phonetic-extensions-supplement
    char-set:block/combining-diacritical-marks-supplement
    char-set:block/latin-extended-additional
    char-set:block/greek-extended
    char-set:block/general-punctuation
    char-set:block/superscripts-and-subscripts
    char-set:block/currency-symbols
    char-set:block/combining-diacritical-mark-for-symbols
    char-set:block/letterlike-symbols
    char-set:block/number-forms
    char-set:block/arrows
    char-set:block/mathematical-operators
    char-set:block/miscellaneous-technical
    char-set:block/control-pictures
    char-set:block/optical-character-recognition
    char-set:block/enclosed-alphanumerics
    char-set:block/box-drawing
    char-set:block/block-elements
    char-set:block/geometric-shapes
    char-set:block/miscellaneous-symbols
    char-set:block/dingbats
    char-set:block/miscellaneous-mathematical-symbols-a
    char-set:block/supplemental-arrows-a
    char-set:block/braille-patterns
    char-set:block/supplemental-arrows-b
    char-set:block/miscellaneous-mathematical-symbols-b
    char-set:block/supplemental-mathematical-operators
    char-set:block/miscellaneous-symbols-and-arrows
    char-set:block/glagolitic
    char-set:block/latin-extended-c
    char-set:block/coptic
    char-set:block/georgian-supplement
    char-set:block/tifinagh
    char-set:block/ethiopic-extended
    char-set:block/cyrillic-extended-a
    char-set:block/supplemental-punctuation
    char-set:block/cjk-radicals-supplement
    char-set:block/kangxi-radicals
    char-set:block/ideographic-description-characters
    char-set:block/cjk-symbols-and-punctuation
    char-set:block/hiragana
    char-set:block/katakana
    char-set:block/bopomofo
    char-set:block/hangul-compatibility-jamo
    char-set:block/kanbun
    char-set:block/bopomofo-extended
    char-set:block/cjk-strokes
    char-set:block/katakana-phonetic-extensions
    char-set:block/enclosed-cjk-letters-and-months
    char-set:block/cjk-compatibility
    char-set:block/cjk-unified-Ideographs-extension-a
    char-set:block/yijing-hexagram-symbols
    char-set:block/cjk-unified-ideographs
    char-set:block/yi-syllables
    char-set:block/yi-radicals
    char-set:block/vai
    char-set:block/cyrillic-extended-b
    char-set:block/modifier-tone-letters
    char-set:block/latin-extended-d
    char-set:block/syloti-nagri
    char-set:block/phags-pa
    char-set:block/saurashtra
    char-set:block/kayah-li
    char-set:block/Rejang
    char-set:block/cham
    char-set:block/hangul-syllables
    char-set:block/private-use-area
    char-set:block/cjk-compatibility-ideographs
    char-set:block/alphabetic-presentation-forms
    char-set:block/arabic-presentation-forms-a
    char-set:block/variation-selectors
    char-set:block/vertical-forms
    char-set:block/combining-half-marks
    char-set:block/cjk-compatibility-forms
    char-set:block/small-form-variants
    char-set:block/arabic-presentation-forms-b
    char-set:block/halfwidth-and-fullwidth-forms
    char-set:block/specials
    char-set:block/linear-b-syllabary
    char-set:block/linear-b-ideograms
    char-set:block/aegean-numbers
    char-set:block/ancient-greek-numbers
    char-set:block/ancient-symbols
    char-set:block/phaistos-disc
    char-set:block/lycian
    char-set:block/carian
    char-set:block/old-italic
    char-set:block/gothic
    char-set:block/ugaritic
    char-set:block/old-persian
    char-set:block/deseret
    char-set:block/shavian
    char-set:block/osmanya
    char-set:block/cypriot-syllabary
    char-set:block/phoenician
    char-set:block/lydian
    char-set:block/kharoshthi
    char-set:block/cuneiform
    char-set:block/cuneiform-numbers-and-punctuation
    char-set:block/byzantine-musical-symbols
    char-set:block/musical-symbols
    char-set:block/ancient-greek-musical-notation
    char-set:block/tai-xuan-jing-symbols
    char-set:block/counting-rod-numerals
    char-set:block/mathematical-alphanumeric-symbols
    char-set:block/mahjong-tiles
    char-set:block/domino-tiles
    char-set:block/cjk-unified-ideographs-extension-b
    char-set:block/cjk-compatibility-ideographs-supplement
    char-set:block/tags
    char-set:block/variation-selectors-supplement
    char-set:block/supplementary-private-use-area-a
    char-set:block/supplementary-private-use-area-b)
  (import (vicare)
    (vicare containers char-sets))



(define char-set:block/basic-latin				(char-set '(#\x0000 . #\x007F)))
(define char-set:block/latin-1-supplement			(char-set '(#\x0080 . #\x00FF)))
(define char-set:block/latin-extended-a				(char-set '(#\x0100 . #\x017F)))
(define char-set:block/latin-extended-b				(char-set '(#\x0180 . #\x024F)))
(define char-set:block/ipa-extensions				(char-set '(#\x0250 . #\x02AF)))
(define char-set:block/spacing-modifier-letters			(char-set '(#\x02B0 . #\x02FF)))
(define char-set:block/combining-diacritical-marks		(char-set '(#\x0300 . #\x036F)))
(define char-set:block/greek-and-coptic				(char-set '(#\x0370 . #\x03FF)))
(define char-set:block/cyrillic					(char-set '(#\x0400 . #\x04FF)))
(define char-set:block/cyrillic-supplement			(char-set '(#\x0500 . #\x052F)))
(define char-set:block/armenian					(char-set '(#\x0530 . #\x058F)))
(define char-set:block/hebrew					(char-set '(#\x0590 . #\x05FF)))
(define char-set:block/arabic					(char-set '(#\x0600 . #\x06FF)))
(define char-set:block/syriac					(char-set '(#\x0700 . #\x074F)))
(define char-set:block/arabic-supplement			(char-set '(#\x0750 . #\x077F)))
(define char-set:block/thaana					(char-set '(#\x0780 . #\x07BF)))
(define char-set:block/nko					(char-set '(#\x07C0 . #\x07FF)))
(define char-set:block/devanagari				(char-set '(#\x0900 . #\x097F)))
(define char-set:block/bengali					(char-set '(#\x0980 . #\x09FF)))
(define char-set:block/gurmukhi					(char-set '(#\x0A00 . #\x0A7F)))
(define char-set:block/gujarati					(char-set '(#\x0A80 . #\x0AFF)))
(define char-set:block/oriya					(char-set '(#\x0B00 . #\x0B7F)))
(define char-set:block/tamil					(char-set '(#\x0B80 . #\x0BFF)))
(define char-set:block/telugu					(char-set '(#\x0C00 . #\x0C7F)))
(define char-set:block/kannada					(char-set '(#\x0C80 . #\x0CFF)))
(define char-set:block/malayalam				(char-set '(#\x0D00 . #\x0D7F)))
(define char-set:block/sinhala					(char-set '(#\x0D80 . #\x0DFF)))
(define char-set:block/thai					(char-set '(#\x0E00 . #\x0E7F)))
(define char-set:block/lao					(char-set '(#\x0E80 . #\x0EFF)))
(define char-set:block/tibetan					(char-set '(#\x0F00 . #\x0FFF)))
(define char-set:block/myanmar					(char-set '(#\x1000 . #\x109F)))
(define char-set:block/georgian					(char-set '(#\x10A0 . #\x10FF)))
(define char-set:block/hangul-jamo				(char-set '(#\x1100 . #\x11FF)))
(define char-set:block/ethiopic					(char-set '(#\x1200 . #\x137F)))
(define char-set:block/ethiopic-supplement			(char-set '(#\x1380 . #\x139F)))
(define char-set:block/cherokee					(char-set '(#\x13A0 . #\x13FF)))
(define char-set:block/unified-canadian-aboriginal-syllabics	(char-set '(#\x1400 . #\x167F)))
(define char-set:block/ogham					(char-set '(#\x1680 . #\x169F)))
(define char-set:block/runic					(char-set '(#\x16A0 . #\x16FF)))
(define char-set:block/tagalog					(char-set '(#\x1700 . #\x171F)))
(define char-set:block/hanunoo					(char-set '(#\x1720 . #\x173F)))
(define char-set:block/buhid					(char-set '(#\x1740 . #\x175F)))
(define char-set:block/tagbanwa					(char-set '(#\x1760 . #\x177F)))
(define char-set:block/khmer					(char-set '(#\x1780 . #\x17FF)))
(define char-set:block/mongolian				(char-set '(#\x1800 . #\x18AF)))
(define char-set:block/limbu					(char-set '(#\x1900 . #\x194F)))
(define char-set:block/tai-le					(char-set '(#\x1950 . #\x197F)))
(define char-set:block/new-tai-lue				(char-set '(#\x1980 . #\x19DF)))
(define char-set:block/khmer-symbols				(char-set '(#\x19E0 . #\x19FF)))
(define char-set:block/buginese					(char-set '(#\x1A00 . #\x1A1F)))
(define char-set:block/balinese					(char-set '(#\x1B00 . #\x1B7F)))
(define char-set:block/sundanese				(char-set '(#\x1B80 . #\x1BBF)))
(define char-set:block/lepcha					(char-set '(#\x1C00 . #\x1C4F)))
(define char-set:block/ol-chiki					(char-set '(#\x1C50 . #\x1C7F)))
(define char-set:block/phonetic-extensions			(char-set '(#\x1D00 . #\x1D7F)))
(define char-set:block/phonetic-extensions-supplement		(char-set '(#\x1D80 . #\x1DBF)))
(define char-set:block/combining-diacritical-marks-supplement	(char-set '(#\x1DC0 . #\x1DFF)))
(define char-set:block/latin-extended-additional		(char-set '(#\x1E00 . #\x1EFF)))
(define char-set:block/greek-extended				(char-set '(#\x1F00 . #\x1FFF)))
(define char-set:block/general-punctuation			(char-set '(#\x2000 . #\x206F)))
(define char-set:block/superscripts-and-subscripts		(char-set '(#\x2070 . #\x209F)))
(define char-set:block/currency-symbols				(char-set '(#\x20A0 . #\x20CF)))
(define char-set:block/combining-diacritical-mark-for-symbols	(char-set '(#\x20D0 . #\x20FF)))
(define char-set:block/letterlike-symbols			(char-set '(#\x2100 . #\x214F)))
(define char-set:block/number-forms				(char-set '(#\x2150 . #\x218F)))
(define char-set:block/arrows					(char-set '(#\x2190 . #\x21FF)))
(define char-set:block/mathematical-operators			(char-set '(#\x2200 . #\x22FF)))
(define char-set:block/miscellaneous-technical			(char-set '(#\x2300 . #\x23FF)))
(define char-set:block/control-pictures				(char-set '(#\x2400 . #\x243F)))
(define char-set:block/optical-character-recognition		(char-set '(#\x2440 . #\x245F)))
(define char-set:block/enclosed-alphanumerics			(char-set '(#\x2460 . #\x24FF)))
(define char-set:block/box-drawing				(char-set '(#\x2500 . #\x257F)))
(define char-set:block/block-elements				(char-set '(#\x2580 . #\x259F)))
(define char-set:block/geometric-shapes				(char-set '(#\x25A0 . #\x25FF)))
(define char-set:block/miscellaneous-symbols			(char-set '(#\x2600 . #\x26FF)))
(define char-set:block/dingbats					(char-set '(#\x2700 . #\x27BF)))
(define char-set:block/miscellaneous-mathematical-symbols-a	(char-set '(#\x27C0 . #\x27EF)))
(define char-set:block/supplemental-arrows-a			(char-set '(#\x27F0 . #\x27FF)))
(define char-set:block/braille-patterns				(char-set '(#\x2800 . #\x28FF)))
(define char-set:block/supplemental-arrows-b			(char-set '(#\x2900 . #\x297F)))
(define char-set:block/miscellaneous-mathematical-symbols-b	(char-set '(#\x2980 . #\x29FF)))
(define char-set:block/supplemental-mathematical-operators	(char-set '(#\x2A00 . #\x2AFF)))
(define char-set:block/miscellaneous-symbols-and-arrows		(char-set '(#\x2B00 . #\x2BFF)))
(define char-set:block/glagolitic				(char-set '(#\x2C00 . #\x2C5F)))
(define char-set:block/latin-extended-c				(char-set '(#\x2C60 . #\x2C7F)))
(define char-set:block/coptic					(char-set '(#\x2C80 . #\x2CFF)))
(define char-set:block/georgian-supplement			(char-set '(#\x2D00 . #\x2D2F)))
(define char-set:block/tifinagh					(char-set '(#\x2D30 . #\x2D7F)))
(define char-set:block/ethiopic-extended			(char-set '(#\x2D80 . #\x2DDF)))
(define char-set:block/cyrillic-extended-a			(char-set '(#\x2DE0 . #\x2DFF)))
(define char-set:block/supplemental-punctuation			(char-set '(#\x2E00 . #\x2E7F)))
(define char-set:block/cjk-radicals-supplement			(char-set '(#\x2E80 . #\x2EFF)))
(define char-set:block/kangxi-radicals				(char-set '(#\x2F00 . #\x2FDF)))
(define char-set:block/ideographic-description-characters	(char-set '(#\x2FF0 . #\x2FFF)))
(define char-set:block/cjk-symbols-and-punctuation		(char-set '(#\x3000 . #\x303F)))
(define char-set:block/hiragana					(char-set '(#\x3040 . #\x309F)))
(define char-set:block/katakana					(char-set '(#\x30A0 . #\x30FF)))
(define char-set:block/bopomofo					(char-set '(#\x3100 . #\x312F)))
(define char-set:block/hangul-compatibility-jamo		(char-set '(#\x3130 . #\x318F)))
(define char-set:block/kanbun					(char-set '(#\x3190 . #\x319F)))
(define char-set:block/bopomofo-extended			(char-set '(#\x31A0 . #\x31BF)))
(define char-set:block/cjk-strokes				(char-set '(#\x31C0 . #\x31EF)))
(define char-set:block/katakana-phonetic-extensions		(char-set '(#\x31F0 . #\x31FF)))
(define char-set:block/enclosed-cjk-letters-and-months		(char-set '(#\x3200 . #\x32FF)))
(define char-set:block/cjk-compatibility			(char-set '(#\x3300 . #\x33FF)))
(define char-set:block/cjk-unified-Ideographs-extension-a	(char-set '(#\x3400 . #\x4DBF)))
(define char-set:block/yijing-hexagram-symbols			(char-set '(#\x4DC0 . #\x4DFF)))
(define char-set:block/cjk-unified-ideographs			(char-set '(#\x4E00 . #\x9FFF)))
(define char-set:block/yi-syllables				(char-set '(#\xA000 . #\xA48F)))
(define char-set:block/yi-radicals				(char-set '(#\xA490 . #\xA4CF)))
(define char-set:block/vai					(char-set '(#\xA500 . #\xA63F)))
(define char-set:block/cyrillic-extended-b			(char-set '(#\xA640 . #\xA69F)))
(define char-set:block/modifier-tone-letters			(char-set '(#\xA700 . #\xA71F)))
(define char-set:block/latin-extended-d				(char-set '(#\xA720 . #\xA7FF)))
(define char-set:block/syloti-nagri				(char-set '(#\xA800 . #\xA82F)))
(define char-set:block/phags-pa					(char-set '(#\xA840 . #\xA87F)))
(define char-set:block/saurashtra				(char-set '(#\xA880 . #\xA8DF)))
(define char-set:block/kayah-li					(char-set '(#\xA900 . #\xA92F)))
(define char-set:block/Rejang					(char-set '(#\xA930 . #\xA95F)))
(define char-set:block/cham					(char-set '(#\xAA00 . #\xAA5F)))
(define char-set:block/hangul-syllables				(char-set '(#\xAC00 . #\xD7AF)))
;;;These are excluded from the definition of characters in R6RS.
;;;(define char-set:block/high-surrogates			(char-set '(#\xD800 . #\xDB7F)))
;;;(define char-set:block/high-private-use-surrogates		(char-set '(#\xDB80 . #\xDBFF)))
;;;(define char-set:block/low-surrogates			(char-set '(#\xDC00 . #\xDFFF)))
(define char-set:block/private-use-area				(char-set '(#\xE000 . #\xF8FF)))
(define char-set:block/cjk-compatibility-ideographs		(char-set '(#\xF900 . #\xFAFF)))
(define char-set:block/alphabetic-presentation-forms		(char-set '(#\xFB00 . #\xFB4F)))
(define char-set:block/arabic-presentation-forms-a		(char-set '(#\xFB50 . #\xFDFF)))
(define char-set:block/variation-selectors			(char-set '(#\xFE00 . #\xFE0F)))
(define char-set:block/vertical-forms				(char-set '(#\xFE10 . #\xFE1F)))
(define char-set:block/combining-half-marks			(char-set '(#\xFE20 . #\xFE2F)))
(define char-set:block/cjk-compatibility-forms			(char-set '(#\xFE30 . #\xFE4F)))
(define char-set:block/small-form-variants			(char-set '(#\xFE50 . #\xFE6F)))
(define char-set:block/arabic-presentation-forms-b		(char-set '(#\xFE70 . #\xFEFF)))
(define char-set:block/halfwidth-and-fullwidth-forms		(char-set '(#\xFF00 . #\xFFEF)))
(define char-set:block/specials					(char-set '(#\xFFF0 . #\xFFFF)))
(define char-set:block/linear-b-syllabary			(char-set '(#\x10000 . #\x1007F)))
(define char-set:block/linear-b-ideograms			(char-set '(#\x10080 . #\x100FF)))
(define char-set:block/aegean-numbers				(char-set '(#\x10100 . #\x1013F)))
(define char-set:block/ancient-greek-numbers			(char-set '(#\x10140 . #\x1018F)))
(define char-set:block/ancient-symbols				(char-set '(#\x10190 . #\x101CF)))
(define char-set:block/phaistos-disc				(char-set '(#\x101D0 . #\x101FF)))
(define char-set:block/lycian					(char-set '(#\x10280 . #\x1029F)))
(define char-set:block/carian					(char-set '(#\x102A0 . #\x102DF)))
(define char-set:block/old-italic				(char-set '(#\x10300 . #\x1032F)))
(define char-set:block/gothic					(char-set '(#\x10330 . #\x1034F)))
(define char-set:block/ugaritic					(char-set '(#\x10380 . #\x1039F)))
(define char-set:block/old-persian				(char-set '(#\x103A0 . #\x103DF)))
(define char-set:block/deseret					(char-set '(#\x10400 . #\x1044F)))
(define char-set:block/shavian					(char-set '(#\x10450 . #\x1047F)))
(define char-set:block/osmanya					(char-set '(#\x10480 . #\x104AF)))
(define char-set:block/cypriot-syllabary			(char-set '(#\x10800 . #\x1083F)))
(define char-set:block/phoenician				(char-set '(#\x10900 . #\x1091F)))
(define char-set:block/lydian					(char-set '(#\x10920 . #\x1093F)))
(define char-set:block/kharoshthi				(char-set '(#\x10A00 . #\x10A5F)))
(define char-set:block/cuneiform				(char-set '(#\x12000 . #\x123FF)))
(define char-set:block/cuneiform-numbers-and-punctuation	(char-set '(#\x12400 . #\x1247F)))
(define char-set:block/byzantine-musical-symbols		(char-set '(#\x1D000 . #\x1D0FF)))
(define char-set:block/musical-symbols				(char-set '(#\x1D100 . #\x1D1FF)))
(define char-set:block/ancient-greek-musical-notation		(char-set '(#\x1D200 . #\x1D24F)))
(define char-set:block/tai-xuan-jing-symbols			(char-set '(#\x1D300 . #\x1D35F)))
(define char-set:block/counting-rod-numerals			(char-set '(#\x1D360 . #\x1D37F)))
(define char-set:block/mathematical-alphanumeric-symbols	(char-set '(#\x1D400 . #\x1D7FF)))
(define char-set:block/mahjong-tiles				(char-set '(#\x1F000 . #\x1F02F)))
(define char-set:block/domino-tiles				(char-set '(#\x1F030 . #\x1F09F)))
(define char-set:block/cjk-unified-ideographs-extension-b	(char-set '(#\x20000 . #\x2A6DF)))
(define char-set:block/cjk-compatibility-ideographs-supplement	(char-set '(#\x2F800 . #\x2FA1F)))
(define char-set:block/tags					(char-set '(#\xE0000 . #\xE007F)))
(define char-set:block/variation-selectors-supplement		(char-set '(#\xE0100 . #\xE01EF)))
(define char-set:block/supplementary-private-use-area-a		(char-set '(#\xF0000 . #\xFFFFF)))
(define char-set:block/supplementary-private-use-area-b		(char-set '(#\x100000 . #\x10FFFF)))


;;; done

)

;;; end of file
