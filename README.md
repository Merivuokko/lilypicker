# LilyPicker

LilyPicker is a [LilyPond](https://www.lilypond.org/) music notation preprocessor.
It streamlines the composition of music by allowing non-sequential writing:
it allows both writing multiple parts on the same line (in a table-like manner) and writing sections of music where there is one line per one part following each other.

## Motivation

LilyPond is a powerful tool for professional music typesetting.
However, writing intermixed voices and parts in LilyPond can be challenging.
Intermixing non-music content (e.g. lyrics) is completely impossible with LilyPond alone.
This capability is particularly beneficial when:
1) The reader of the music wishes to view simultaneous events in the source.
2) The writer prefers to compose or transcribe music incrementally.

LilyPond's `\parallelMusic` command offers some assistance, yet it acts more as a preliminary solution rather than a fully-fledged feature.

LilyPicker expands on this by providing a straightforward text format that is both simple to read and write.
It produces LilyPond notation, making it ideal for inclusion in a processing pipeline alongside LilyPond itself.

LilyPicker operates principally as a basic preprocessor, avoiding any attempt to interpret LilyPond syntax.
This approach is the root of both its strengths and limitations:
LilyPond is a huge language and parsing it fully is an enormous task.

## Status

The developer uses LilyPicker daily for their music theory studies.
It should be ready for production-use if its features are powerful enough for the task at hand.

LilyPicker's parser is robust and it produces sensible error messages.
The LilyPond printer preserves the original line layout.
In addition to LilyPond notation, LilyPicker outputs `\sourcefilename` and `\sourcefileline` directives which LilyPond understands.
This means that LilyPond markup errors are mostly associated correctly to the original input file positions.

Note, however, that elementary syntax errors such as failing to close a quotation can cause LilyPond to misinterpret large sections of music which may be very confusing.

## Format

LilyPicker files consist of typical LilyPond markup complemented by extensions that enable a more flexible approach to writing music.

Mostly, the input passes through to LilyPond unchanged, but LilyPicker also recognizes special directives to manage the sequencing of output.
Lines in LilyPicker files begin (without any leading whitespace) with a directive — often a single character — indicating the line's function.

### Part List Specification

Lines starting with an equal sign (`=`) denote part definition.
The rest of the line lists part names, each separated by a vertical bar (`|`) and associated with a LilyPond command that wraps the part’s content.

Subsequent lines starting with a bar (`|`) group music to the specified parts and assign it to respective LilyPond variables.

While part names must be valid LilyPond variable identifiers, special characters are accommodated via quotation marks in the output.

For instance:

```lilypicker
    = VlI \fixed c' | Vlc
    | c4 e g c'     | e4 c g, c
    | g4 e c2       | e4 g, c,2
```

Produces approximately the following output (ignoring the layout):

```lilypond
    VlI = \fixed c' { c4 e g c g4 e c2 }
    Vlc = { e4 c g, c, e4 g, c,2 }
```

This directive introduces new parts.
For modifications to existing parts, employ the Part extension syntax.

### Part Extensions

Lines starting with a greater-than sign (`>`) revise the active part list.
Subsequently, parallel music lines add music to the parts specified in this directive.

Part naems to be activated are separated by bars (|).
The named parts need to have been defined beforehand with a Part definition directive.

### Parallel Music

Lines starting with a bar (`|`) distribute music among the currently active parts, using additional bars as part separators.
Music is appended to the currently active parts in the order given by the most recent part name definition or part extension line.

Empty parts are allowed and they are replaced by skips in the output.
This is especially useful for incremental music writing (e.g. while doing a dictation in a solfege class).
In this case all music expressions (separated by bars) are expected to have the same length.
If this is not the case, filling empty sections with skips is not possible and the output will be incorrect.
The length of the skip is derived from the length of the first non-empty music expression on the same parallel music line.
This feature interacts very badly with lyric mode and other modes, which don't understand \skip.

If a parallel music field (text between bar lines) contains only a single hyphen (`-`), no output is produced.
This can be used to work around unwanted consequences of automatic skip insertion.

Parallel music definitions do not need to contain just music expressions.
For example lyrics can also be intermixed with the music with this directive.
This requires that a proper mode is enabled on a part list definition line.

If the parallel music line begins with two consecutive bars (`||`) a bar check is prepended to all music expressions on this line.
Similarly, two bars may also be used in the end of the parallel music line to append a bar check to every part's output.

### Adding music to individual parts

Music can be added to multiple individual parts by starting a line with a space-separated list of part names, followed by a colon (:), which in turn shall be followed by some music.
The music is added to all the named parts regardless of their activity status.

### Adding music to all parts

If a line begins with an asterisk (\*), the rest of the line is added to every active part.

### Adding text verbatim

If a line begins with a minus sign (`-`), the following text is added verbatim to the beginning of LilyPicker's output.
Lines prepended with this syntax appear in the order they are read from input.

If a line begins with a plus sign (`+`), the following text is appended verbatim to the end of LilyPicker's output.

### Including other files

The `#include` directive may be used to include text from other files.
The file name to be included shall follow the directive on the same line and should be one word (which means it may need to be quoted).

The contents of the line are parsed exactly the way it would be, if inserted to the original input stream in place of the `#include` line.

### Comments

Lines beginning with a percent sign (%) are comments.
They are ignored by LilyPicker and will not appear in its output.
If you wish to emit a comment to the LilyPond output, embed the comments to other lines.

### Escaping special characters

You may escape the `|`, `"` and `:` characters on part definition lists, parallel music lines and part names by prepending them with a dollar sign (`$`).
To output a dollar sign on these lines, use `$$`.
Double quotes may also be used to have spaces inside variable names.
The escape character `$` works inside quotes in a normal way.

## Example

Here is a short example of a simple LilyPicker file.

```lilypicker
    % This snippet produces one measure of choral music.
    - \version "2.24.2"
    = bassus | tenor | altus | cantus
    | c2 g c1 | e'2 d' c'1 | a'2 g' e'1 | a'2 h' c''1 ||
    * \bar "|."
    + \include "satb-choir-template.ly"
```

It produces roughly the following output:

```lilypond
    \version "2.24.2"

    bassus = {
      c2 g c1 |
      \bar "|."
    }
  
    tenor = {
      e'2 d' c'1 |
      \bar "|."
    }

    altus = {
      a'2 g' e'1 |
      \bar "|."
    }

    cantus = {
      a'2 h' c''1 |
      \bar "|."
    }

    \include "satb-choir-template.ly"
```
