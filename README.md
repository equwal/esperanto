# Esperanto morphological compression

This is an experimental program to compress Esperanto language using its quality
_highly regular morphology_. Most NLP compression methods require an analytic
language (eg. English) to work, but this it different.

*Warning: may not be functional at this point.* Pull requests appreciated.


## Guts
There are two metrics that matter with the compression: the percent reduction in
size using just morphological compression, and the reduction when combined with
another NLP compression method.

TODO: Use [private use
unicode](http://www.unicode.org/faq/private_use.html#pua2) for marking purposes.
