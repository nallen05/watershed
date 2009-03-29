# Watershed

> _WATERSHED_ is a term used to describe a time in television schedules during which
>  it is permissible to show television programmes which have 'adult content'. Adult
>  content can be generally defined as having nudity, explicit sexual intercourse,
>  graphic violence, strong language, or drug references or use.
 
   _--http://en.wikipedia.org/wiki/Watershed_(television)_
 
 _Watershed_ is a down and dirty way of producing HTML with lisp. Its goal is to
 be the fasted possible path from static html to dynamicly generated webpage.
 
Using watershed templates to make a deadline is something like solving a war with
chemical weapons: It may solve the problem temporarily, but it will cause you and
your neighbors problems down the road and the rest of the "civilized" international
community will look down on you for doing it.

The problem is, of course, that using watershed means writing "throwaway code" and
creating a website that will be almost completely impossible to maintain or change
at a later date.
 
You have been warned.
 
## TEMPLATE SEMANTICS
 
Instead of embedding `<?php ... ?>` elements in HTML like php, watershed simply uses
uses `(` and `)` to switch in and out of Common Lisp from HTML.
 
The following php file:
 
    <html>
      <head>
        <title>test</title>
      </head>
      <body>
        <?php echo '<p>Hello World</p>'; ?>
      </body>
    </html>
 
is like the following watershed file:
 
    <html>
      <head>
        <title>test</title>
      </head>
      <body>
        (format *html-output* "<p>Hello World</p>")
      </body>
    </html>
 
(of course you'd want to use one of the template functions, not `FORMAT` to write
stuff to the client's browser...)
 
 
The "special operators" `HTML` and `XML` switch back back to html/xml mode.
 
So you could use something like:
 
    <html>
      <head>
        <title>test</title>
      </head>
      <body>
        (let ((place "World"))
    
             ;; in lisp mode
             (html
    
    	       <!-- back in html mode -->
    	       <p>(format *html-output* "Hello ~A" place)</p>
    
    	    ))
      </body>
    </html>
 
Use the escape character `\` to insert literal `)`, `(`, or `\` characters.
 
## PACKAGES
 
Watershed'understands toplevel `IN-PACKAGE` forms, so just put one somewhere near the
top of your watershed file like you would with a ".lisp" file.
 
The default package inside watersehd templates is the `:WATERSHED-USER` package,
which uses `:CL` and `:WATERSHED` and exports nothing.
 
## ENCODING
 
Watershed'assumes template files are encoded in UTF-8.
 
## EMACS MODE
 
Watershed should come with "watershed-mode.el" which automatically switches a
between lisp mode and some other mode--by default html-mode--according to watershed
semantics. So the buffer starts in html-mode, when the point enters a sexp it
switches to lisp-mode, when it enters an HTML or XML operator it switches back to
HTML mode, etc... This enables you to use tab-completion, M-., function signitures,
etc, inside templates.
 
It should also come with "superior-watershed-mode.el" which provides the elisp
function `WATERSHED-COMPILE-BUFFER`, which compiles a watershed template buffer with
with SLIME (like `SLIME-COMPILE-FILE` except no fasl involved) and displays the notes.

_note: I have not used watershed since switching to Djula, the slime/emacs
funcionality is currently broken if I remember correctly :-/ --Nick_
 
## utf-8 breaks my SLIME/Emacs
 
Put  this in your .emacs file:

     (set-language-environment "UTF-8") 
     (setq slime-net-coding-system 'utf-8-unix) 
 
##  TEMPLATE API
 
* `*HTML-OUTPUT*`

- - -
 
  _Variable_ bound to a character output stream when rendering templates.
  Write characters to this to send stuff to the user's browser.
 
* `RAW (THING)`

- - -
 
  _Function_. `PRINC`'s `THING` to `*HTML-OUTPUT*`
 
* `HE (THING)`

- - -
 
  _Function_. `PRINC`'s `THING` to `*HTML-OUTPUT*`, html-escaping it on the way out
 
* `HE-TO-STRING (THING)`

- - -
 
  _Function_. `PRINC`'s `THING` to a string then html-escapes it.
 
*  `UE (THING)`

- - -
 
  `PRINC`'s `THING` to `*HTML-OUTPUT*`, url-encoding it on the way out.
 
   _Note: assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, 
   which probably assumes LATIN-1 by default (see `HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*`)._
 
* `UE-TO-STRING (THING)`

- - -
 
   _Function_. `PRINC`'s `THING` to a string then url-encodes it.
 
    _Note: assumes UTF-8 so be careful if seen around `HUNCHENTOOT:URL-DECODE`, 
    which probably assumes LATIN-1 by default (see `HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*`).
 
* `UD (STRING)`

- - -
 
   _Function_. Returns the url-decoded version of `STRING`.
 
   _Note: assumes UTF-8 so be careful if seen around `HUNCHENTOOT:URL-DECODE`, which
   probably assumes LATIN-1 by default (see `HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*`).
  
* `HTML (&REST HTML)`

- - -

   _Macro/special-operator_. When used in normal code, `HTML` is a macro that writes
   any literal strings you give it to `*HTML-OUTPUT*`
 
   When seen in a watershed template, HTML sends the parser back into non-lisp mode
   mode until the appropriate closing `)`
 
*  `XML (&REST XML)`

- - -

   _Macro/special-operator_. Synonym for `HTML` for the other type of people in the
    world.
 
##  LISP API
  
* `COMPILE-WATERSHED-TEMPLATE (path &key (type :template) (enctype :utf-8) (compile t) to-bytes)`

- - -
 
  _Function_. Returns a thunk (0-argument function) that, when called, renders the
  watershed template pointed to by the file designator `PATH`.
 
  If `TYPE` is `EQL` to the keyword:
 
  -`:TEMPLATE`, then `PATH` is assumed to be a watershed template
 
  -`:LISP` then `PATH` is assumed to contain lisp code (without template semantics).
    When the function is called, the lisp code will be evaluated with `*HTML-OUTPUT*`
    bound to a string-output-stream, so all the template API should work (except `HTML`
    and `XML` will just be macros and not "special operators").
 
  - `:LISP-NO-BUFFER`, then `PATH' is assumed to contain lisp code (without template
    semantics). When the function is called, the lisp code will be evaluated.
    `*HTML-OUTPUT*`  will _not_ be bound, so none of the template API that depends
     on `*HTML-OUTPUT*` will work.
 
  - `:TEMPLATE`, `:LISP`, and `:LISP-NO-BUFFER` all understand top-level `IN-PACKAGE`
    forms, so just use them like you would in a normal ".lisp" file.
 
    The default package is `:WATERSHED-USER.`
 
   If `COMPILE` is `NIL` then `COMPILE-WATERSHED-TEMPLATE` just returns the lisp
   source code of this function, not a compiled function.
 
   If `TO-BYTES` is `T`, the funtion returns UTF-8 bytes instead of a string when
   called.
