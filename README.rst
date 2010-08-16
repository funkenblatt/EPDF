Emacs PDF Library
============================

:Author: Jason Feng
:Copyright: This document has been placed in the public domain.

This library is intended to allow elisp programs to deal with PDF
files.  The main motivation behind this is to allow users to do such
things as viewing the outlines and annotations of a PDF file while
using docview-mode.

Things it does not do:

* Work in a robust manner
* Handle character encodings
* Deal with encrypted documents of any kind
* Anything particularly useful

I'm concentrating on that last bullet item at the moment.

At the moment, one thing it can be made to do is display a particular
PDF document's outline in outline-mode format, allowing you to use whatever
outline-mode key bindings may exist, while also allowing you to jump to
the proper page by hitting Enter.  

Currently, the way to bring up this functionality is to open up your
document in doc-view, and then eval (pdf-outlines (pdf-init)), where
(pdf-init) returns a document structure, and pdf-outlines dumps the
outlines into a buffer and activates outline-mode.

A screenshot of this functionality
is shown `here`_.

__
.. _here: http://jason.ozbert.com/images/pdf_outline.png
