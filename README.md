# Lily Picker

Lily Picker is a LilyPond music notation preprocessor.
It facilitates writing music non-sequentially – i.e. multiple parts on one line or one line per one part intermixed with other parts.

## Format

LilyPicker files contain regular LilyPond markup except that there are extensions which facilitate laying out music in more relaxed ways.

LilyPicker is mostly a dummy preprocessor.
It does not attempt to understand LilyPond syntax.
LilyPicker's advantages and weaknesses all stem from this strategy.
LilyPond is a complicated language and attempting to parse it would be a huge task.

LilyPicker passes most of its input to LilyPond unaltered, but it supports various directives that may be used to control the order of output.
Each line of a LilyPicker file starts (without any whitespace) by a directive (often a single character) which defines how the line is to be interpreted.

### Part list specification

Lines starting with an equals sign (=) are part name definitions.
The rest of the line is a list of part name definitions separated by bars (|).
Part name definitions consist of the part name and a LilyPond command to wrap the music of the part in.

After this directive, parallel music lines (starting with a bar (|)) add contents to these named parts.
This music is bound to a LilyPond variable.
Thus part names have to be valid LilyPond variable names.
However, variable names are quoted in output as needed so special characters can be used.

After a variable name a LilyPond music function may be specified (separated from the name by a space).
This function is used to wrap the music entered into this part.
Parameters to this function may be defined as well.

For example the following input:

```
    # VlI \fixed c' | Vlc
    | c4 e g c      | e4 c g, c,
```

produces the following output:

```
    VlI = \fixed c' { c4 e g c }
    Vlc = { e4 c g, c, }
```

This command defines new parts that have not been defined already.
If you want to change the currently active parts' list, use Part extension syntax.

### Part extensions

Lines starting with a greater than sign (\>) change the list of currently active parts.
Thus the parallel music directives following a part extension directive add music to the parts named in the extension directive.

Part naems to be activated are separated by bars (|).
The named parts need to have been defined beforehand with a Part definition directive.

### Parallel music

Lines starting with a bar (|) contain music that is to be split into separate parts.
Each following | is a part separator.
Music between the bars is appended to the currently active parts in the order given by the most recent part name definition or part extension line.

Empty parts are allowed and they are replaced by skips in the output.
In this case all sections (separated by bars) are expected to have the same length.
If this is not the case, filling empty sections with skips is not possible and the output will be incorrect.
The length of the skip is derived from the length of the first non-empty music expression on the same parallel music line.

Parallel music definitions do not need to contain music expressions.
For example lyrics are also possible.
This requires that a proper mode is enabled on a part list definition line.

If the parallel music line begins with two consecutive bars (||) a bar check is prepended to output produced by this line for every part.
Two bars may also be used in the end of the parallel music line to append a bar check to every part's output.

### Adding music to all parts

If a line begins with an asterisk (\*), the rest of the line is added to every active part.

### Adding text verbatim

If a line begins with a minus sign (-), the text after that is added verbatim to the beginning of LilyPicker's output file.
Lines prepended with this syntax appear in the order they are read from input.

If a line begins with a plus sign (+), the text after that is appended verbatim to the end of LilyPicker's output file.

### Comments

Lines beginning with a percent sign (%) are comments.
They are ignored by LilyPicker and will not appear in its output.
If you wish to emit a comment to the LilyPond output, embed the comments to other lines.

### Escaping special characters

You may escape the | character on part definition list and parallel music lines by prepending it with a dollar sign ($).
To output a dollar sign on these lines, use $$.

## Example

Here is a short example of a simple LilyPicker file.

```
    % This snippet produces one measure of choral music.
    - \version "2.24.2"
    = bassus | tenor | altus | cantus
    | c2 g c1 | e'2 d' c'1 | a'2 g' e'1 | a'2 h' c''1 ||
    * \bar "|."
    + \include "satb-choir-template.ly"
```

It produces roughly the following output:

```
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
    * 
\include "satb-choir-template.ly"
```

## Alignment of output

LilyPicker tries to align the output such that LilyPond's error messages would make sense to the user.
This works mostly as expected.
