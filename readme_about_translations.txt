﻿Notes about 5.2 release:

Modified existing strings in === PeaLauncher text group ===
txt_4_5_search: Search

Please take time to check the === about text group === against default.txt, in order to check if the localized mini-tutorial is up to date.

It should not be relevant for non-English languages, but alongside some minor corrections please note the term "job" was replaced by "task" for clarity and uniformity starting from release 5.1. This change apply only for the text: variable names containing "job" string are not changed.

Some translations were developed on quite outdated PeaZip versions and needs to be updated:
Chinese Traditional - 繁體中文
German - Deutsch
Korean - 한국어
Dutch - Nederlands
Romanian - Română
Sinhala - සිංහල
Turkish - Türkçe
Vietnamese - Việt



General notes for translators:

IMPORTANT:
the spreadsheet contains 3 pages, "PeaZip text group", "PeaLauncher text group", and "About text group": all pages need to be completed and pasted (column E, for first two pages) in the language file; the "About text group" can be freely edited.
if instead you are directly translating a .txt language file, please remember to check for update all the tree section of the file, marked by the strings === PeaZip text group ===, === PeaLauncher text group ===, and === About text group === respectively.

Language files are UTF-8 encoded text files which can be edited using any suitable text editor.
To create a new translation file you can:
1 - copy default.txt (in PeaZip's path in /res/lang subfolder) or any other language file, if you prefer starting from another language, to a new file;
2 - edit lines 2 to 6 of the document to enter language name (both in English and in the original language for better readability), PeaZip's version (major.minor) the translation is aimed to, translator's and last last revisor's name and last revision date;
3 - translate the text after the "variable_name: " part in "=== PeaZip text group ===" AND "=== PeaLauncher text group ===" sections of the file (don't move or remove lines, don't change the "variable_name: " part);
4 - optionally, translate the mini-tutorial after "=== about text group ===" line (free editing, it is loaded and displayed "as is" as application's mini-tutorial); it is very important to improve usability of the program for non-English speaking users.

In "PeaZip translations" download page, there is a package named peazip-x.y.about_translations.zip containing a spreadsheet file to help in creating and maintaining localizations, simply compiling column D of the spreadsheet.

The spreadsheet shows variable name (column B), corresponding text string in english (column C) and a blank, yellow column (D) for typing the translated text strings.
On the right, a column E (blue) will show the "variable_name: " part assembled with the translated string: the content of this area can be copied and paste to replace the text in "=== PeaZip text group ===" and "=== PeaLauncher text group ===" sections (the spreadsheet features TWO pages, one for each of the two groups).
Lines must be pasted in the original order (it is sufficient to sort them by column F).

After column F are featured all currently available translations, in order to help translators more proficient in other languages than Englis, and to help to spot out what localizations need to be updated.
At each version all language files are mass-updated, with missing text lines in English; to update a localization, it's enough to update the English text lines.
For a better result it is also recommended to check all the language file to see if the update is coherent with linguistic style used by the translator of the current version.
For languages spoken in different ways in different countries (i.e. English, Spanish, Portuguese...) it is recommended to fork the translation, creating i.e. en-us, pt-br etc

PeaZip can load out of order (not optimal for performances) language files for older or newer versions.

Translated language files can be sent to giorgio.tani.software@gmail.com, to be evaluated for inclusion in future updates or publication in https://sourceforge.net/projects/peazip/files/Translations/
All translated language files should be considered as released under GFDL, GNU Free Documentation License, as they have to be considered derivate work from the application's language file which is released under GFDL.
Default language file is default.txt