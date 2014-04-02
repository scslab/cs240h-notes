# CS240h Scribe Notes
### David Mazieres, Bryan O'Sullivan, and David Terei

Class notes for the 2014 Spring quarter iteration of this class
written by different students for each lecture.

Class homepage is [here](http://www.scs.stanford.edu/14sp-cs240h/).

## Writing Notes

Notes should be taken in markdown format and processed by pandoc. To
install pandoc, run:

    cabal install pandoc -fhighlighting-kate

Next, to get access to the notes repository, run:

    git clone git@github.com:scslab/cs240h-notes.git

Create a separate subdirectory for each lecture, starting with a
two-digit lecture number.  The markdown lecture file should end with
`.md`.  For example, the basics lecture should be called
`01-basics/basics.md`.  You can place any additional relevant files
(such as images) in the same directory.  You can rebuild the full set
of notes by running `make` in the top level directory, and opening the
resulting `index.html` in your browser.

## Submitting your notes

Once you've written up your notes, please do one of the following:

* Submit a pull request through Github and email cs240h-staff.

* Email cs240h-staff the URL of a git repository from which we can
  pull your notes.

This will allow us to integrate them with the rest of the class.

## Writing Markdown

A description of markdown format is available in the [pandoc README
file][pandoc-README]. Markdown is a simple format intended to be as
similar as possible to plain text. The most useful forms of markup
available in markdown:

* To include a block of Haskell source code, include it between
  lines of at least three tildes, adding `{.haskell}` after the
  start, like so:

    ~~~~~
    ~~~ {.haskell}
    main :: IO ()
    main = putStrLn "hello world"
    ~~~
    ~~~~~

    which results in nicely syntax highlighted code, like this:

    ~~~ {.haskell}
    main :: IO ()
    main = putStrLn "hello world"
    ~~~

* If the code block is not Haskell source (e.g., you are showing GHCi
  interaction), simply omit the `{.haskell}`.

* To include inline code, surround it with backticks.  For example:

    ~~~
        The code `div a b` throws an exception if `b == 0`.
    ~~~

    results in: The code `div a b` throws an exception if `b == 0`.

    If your code starts or ends with a backtick, you must start or end
    with more backticks.  For example:

    ~~~
        In Haskell, placing backticks around a function, e.g., 
        `` `div` ``, turns it from a prefix function to an infix
        function.
    ~~~

    Which yields: In Haskell, placing backticks around a function,
    e.g., `` `div` ``, turns it from a prefix function to an infix
    function.

* Sections are started by line beginning with `#`.  Subsections by a
  line starting with two `#` characters.  For example:

    ~~~
        # Section name
        ...
        ## Subsection name
    ~~~

* To emphasize text, surround it with `*` or `_`.  For strong
  emphasis, use `**` or `__`.

    ~~~
        This is *emphasized*, while this is **strongly emphasized**.
        You can use underscores and get _the same effect_.
    ~~~

    This is *emphasized*, while this is **strongly emphasized**.  You
    can use underscores and get _the same effect_.

* To include a hyperlink, the syntax is:
  `[text-of-link][name-of-link]`, where someplace else in your file
  you have a line declaring the link destination for `name-of-link`.
  Alternatively, you can use `[text-of-link](link destination)`.
  For example the following two examples:

    ~~~
        Use [the pandoc tool][pandoc] to format slides.

        [pandoc]: http://johnmacfarlane.net/pandoc/
    ~~~

    ~~~
        Use [the pandoc tool](http://johnmacfarlane.net/pandoc/) to
        format slides.
    ~~~

    Both produce: Use [the pandoc tool][pandoc] to format slides.

* You can comment text out using an html comment (e.g.,
  `<!-- ... ignore this -->`).

* Finally, if you want to be really fancy, you can include an image by
  placing a `!` before a link.  In a paragraph, the link text becomes
  the alt text, but if the image is its own paragraph, the link text
  becomes the caption.

    ~~~
        ![caption of this weird image](example.svg)
    ~~~

    Produces:

    ![caption of this weird image](example.svg)

    You are by no means expected to draw figures, but if you feel like
    doing so, we recommend using [inkscape][inkscape] to produce SVG
    images.

[pandoc]: http://johnmacfarlane.net/pandoc/
[pandoc-README]: http://johnmacfarlane.net/pandoc/README.html
[inkscape]: http://www.inkscape.org
