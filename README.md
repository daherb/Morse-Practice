# Morse-Practice

Generates random exercise text to practice morse code using word lists.

The word lists included are based on 
 * English: words_alpha.txt found on https://github.com/dwyl/english-words
 * German: wortliste.txt found on https://github.com/davidak/wortliste
 * Swedish: Folkets Lexikon folkets_sv_en_public.xml found on https://folkets-lexikon.csc.kth.se/folkets/om.html

Install:

Installation using either Cabal or Stack. Stack is recommende because it easily handles all dependencies.
To install run `stack install`.

Usage:
```
Morse-Practice: lang no.words wav-file (letters) (txt-file)

where
	lang: language of the wordlist, i.e. en, de or sv
	no.words: number of words in the text to be created
	wav-file: the output file to be written
	letters: optional list of letters that should be practiced.
		If this list contains punctuation or digits,
		they will be randomly generated between words
	txt-file: optional output file for the text
```

License:
The word lists are as far as I can see CC (see corresponding webpages) and this code is licensed under Artistic License 2.0 (see LICENSE file)

Copyright (c) 2020 Herbert Lange
