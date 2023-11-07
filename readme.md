Have you ever wished someone would walk up to you and say, in a tremendously exaggerated, stereotypical surfer voice, "nice data, dude"? Well, wish no longer because now your data can be Nice by definition, due to our patented Manipulative Marketing Naming technique. Introducing!

# Nice Data: There's no Escape

```nice
# this is an example of some Nice data.
project:
    name: Nice data
    description:
        | A file format for storing structured data. Nice uses syntactic whitespace
        + to represent the data structure. It defines two types of data, scalars and
        + strings, which are used to compose its two data structures, lists and maps.
        >
        > Nice to write, Nice to read.
    inspiration:
        - { name: NestedText, url: https://nestedtext.org }
        - { name: YAML,       url: https://yaml.org }
    non-goals: [ general-purpose data serialization, world domination ]
    epic freaking funny number lol: 42069580089001421337666
```

Nice Data is a format for storing structured data in a file. It's pleasant to read and adheres to the philosophy that form should match structure. It's heavily inspired by [NestedText], though it also looks similar to [YAML].

## Syntax

For the purposes of illustration, the following syntax examples are accompanied by their corresponding JSON representation. If you are not already familiar with JSON syntax, I would certainly like to know how you got here.

### Comments

A comment is any line starting with the octothorp, or perhaps the number sign or pound sign or hash or sharp symbol, followed by a space. The space is not optional and it is a syntax error if it is omitted. The comment continues to the end of a line. Comments must occupy their own line, and they do not need to respect the document indentation (though this is strongly encouraged). Comments should be considered to be "attached" to the line below them.

```nice
# this is an example of a comment
# nothing stops the creation of multiple adjacent comments
# it's good to explain what you are doing
```

### Scalar Values

A scalar value is a sequence of valid UTF-8 codepoints. Scalars cannot contain leading or trailing spaces (specifically ASCII `0x20`) or the ASCII linefeed character but otherwise may contain any valid printing character. A scalar is essentially a UTF-8 string, but in a way that indicates that the parser can interpret it as a different type, if necessary. The following are a few examples of valid scalar values (note that a scalar may not occupy more than one line).

- `100`
- `spaces inside the scalar are no problem`
- `2023-10-19 07:16:38Z`

### String Values

A string value is very similar to a scalar value, except that it is started by a leader character sequence and ended with trailer character sequence. Strings may be spread across multiple lines (here, we call each line a string fragment), and each fragment must start with a leader and end with the trailer. Strings fragments respect leading whitespace (after the leader sequence), unlike scalars. The trailer may be used to include trailing whitespace in a fragment. Comments may be interspersed between the fragments that compose a string (demonstrated below).

The string leader sequence consists of an ASCII character followed by a single ASCII space. The space must be omitted if the fragment contains no other characters (because otherwise it would be trailing whitespace, which is forbidden). The leader sequence defines how the fragments of the string are concatenated together, as follows:

- `| ` specifies that this fragment of the string should be directly concatenated onto the previous fragment.

   ```nice
   | ABCDEFGHIJKLM
   | NOPQRSTUVWXYZ
   ```

   parses to the string `"ABCDEFGHIJKLMNOPQRSTUVWXYZ"`

- `+ ` specifies that this fragment of the string should have a space prepended to it and then be concatenated onto the previous fragment.

   ```nice
   | hello
   + to the
   + world
   ```

   parses to the string `"hello to the world"`. This also demonstrates that different fragment leaders may be intermixed.

- `> ` specifies that this fragment of the string should have a linefeed character prepended to it and then be concatenated onto the previous fragment.

   ```nice
   > my
   # the leading space in this fragment is preserved
   >  multiline
   >
   > string
   # this is used to add a trailing newline
   >
   ```

   parses to the string `"my\n multiline\n\nstring\n"`.

Note that the leader of the first fragment of a string has no effect on the string, and may be any of the three options, but using `| ` is recommended.

The standard fragment trailer is just the normal literal linefeed character `"\n"`, as shown in the examples above. However, because Nice does not permit trailing whitespace syntactically, a string fragment may use the pipe character `|` as a trailer. If the last character in a string fragment is `|`, it will be stripped from the fragment while preserving the rest of the line. If a string fragment needs to end with a pipe character, the pipe must be doubled, as the last `|` will be stripped from the fragment.

```nice
| lots of   |
| space
# to end a string line with a pipe character, it must be doubled. Pipes within
# the line are not special in any way
> | many | pipes | abound ||
```

parses to the string `"lots of   space\n| many | pipes | abound |"`.

Consider also that composing the above rules, an empty string fragment may be represented either with `|` or with `| |`. The former is preferred.

### Lists

A list is an ordered sequence of values. These values may be scalars, strings, other lists, or maps. List items are introduced with the sequence `- ` (ASCII minus followed by a space). Similar to string fragment leaders, if a list item is empty, the trailing space of the introducer must be omitted. Comments may be interspersed between subsequent list items. An example:

```nice
- a list
# this is an inline string
- > containing
# this is an empty list item
-
-
    | several
    + values
```

parses to the following JSON structure:

```JSON
["a list", "containing", "", "several values"]
```

There are a couple of new concepts here. The first new concept is demonstrated in the second value, which is an inline string. This is a standard string fragment that appears on the same line after another introducer (either a list item introducer, as in this example, or a map key introducer, which will be demonstrated in the section describing maps). The only difference between an inline string and a normal string as discussed above is that the inline string is composed of only a single fragment (meaning it cannot be spread across multiple lines). The string leader used has no effect on an inline string, since the leader is not applied.

The other new concept is structural indentation. The fourth list item contains an indented string following a list item introducer that does not contain an inline value. Because the string sequence is indented, it belongs to the list item introduced immediately before it. Note that an indented sequence following an introducer that contains an inline value is a syntactic error. That is, the following document **cannot** be parsed:

```nice
- inline value
    > invalid subsequent indented value
```

Indentation is how all parent-child structural relationships are represented in Nice. Here's an example of nesting multiple lists:

```nice
- start the parent
-
    - this is a child item
    -
        - grandchild here
    - back to the child
    -
        - another grandchild
- finish the parent
```

which parses to the following JSON structure:

```JSON
[
    "start the parent",
    [
        "this is a child item",
        [
            "grandchild here"
        ],
        "back to the child",
        [
            "another grandchild"
        ]
    ],
    "finish the parent"
]
```

The Nice document is similar in layout to its indented JSON counterpart but contains somewhat less bracketry.

### Inline Lists

Inline lists allow a list to be specified in a more concise form on a line following another item introducer (either a list item introducer or a map item introducer). They consist of a comma-separated sequence of scalars within a pair of square brackets (`[` and `]`). Inline lists may also contain other inline lists and inline maps (discussed later), but they cannot contain strings. Whitespace before and after values in an inline list is ignored, though whitespace within a value is preserved. Inline list values may not contain commas. For reasons related to intellectual bankruptcy, `[]` and `[ ]` are distinct values, just as they are in NestedText. `[]` represents an empty list, while `[ ]` represents a list containing a single empty string. As is hopefully suggested by the name, an inline list *must* be specified on a single line.

Inline lists are provided for when some parts of a document may benefit from having horizontal layout rather than vertical layout. It can also be used tactically to improve readability in other ways, but should not, in general, be preferred over standard lists. Here's the previous example, with a bit less indentation thanks to use of inline lists:

```nice
- start the parent
-
    - this is a child item
    - [ grandchild here ]
    - back to the child
    - [ another grandchild ]
- finish the parent
```

Of course, this document could be represented using inline lists exclusively, but this is not recommended:

```nice
[ start the parent, [ this is a child item, [ grandchild here ], back to the child, [ another grandchild ] ], finish the parent ]
```

Hopefully you agree that readability suffers when a more complex hierarchy is jammed into an inline list. However, judicious use can dramatically improve readability, such as in the case of representing a 2D data structure with a list of lists:

```nice
- [  1,  2,  3,  4,  5,  6 ]
- [  7,  8,  9, 10, 11, 12 ]
- [ -1, -2, -3, -4, -5, -8 ]
```

### Maps

A map is a data structure consisting of a sequence of pairs, with each pair being composed of a key and value. A map may represent a general-purpose pair-based data structure such as a hashtable, or it may represent a strictly defined data type with a fixed number of named fields, like a C `struct`. The keys of the map are exclusively scalars, but the corresponding values may be any Nice type or scalar, including scalars, strings, lists, or other maps.

A map item is introduced by the key scalar. A key scalar is a scalar value that is terminated with an ASCII colon followed by a space `: `. The `:` is removed from the end of the key scalar when parsing. Key scalars may not begin with a sequence that is used for introducing a different type, which means that map keys cannot start with `#` (comments), `- ` (list item introducer), `+ `, `| `, `> ` (string fragment leaders), `[` (inline lists), or `{` (inline maps). `-`, `+`, `|`, and `>` without a following space may be used to begin map keys unambiguously, but `#`, `[`, and `{` are always forbidden. Additionally, key scalars may not contain a colon `:`. Comments may intersperse map pairs. As with the other introducers, if the key scalar is the only item on a line, it must not have a trailing space.

Enough talk, have an example:

```nice
a scalar: value
a string:
    | hello
    + from a map
inline string: | hello from a map
a list:
    - true
    - false
    - null
inline list: [ 1, 2, 3 ]
a map:
    nested:
        several: levels
an empty value:
```

This maps to the following JSON structure:

```JSON
{
    "a scalar": "value",
    "a string": "hello from a map",
    "inline string": "hello from a map",
    "a list": ["true", "false", "null"],
    "inline list": ["1", "2", "3"],
    "a map": { "nested": { "several": "levels" } },
    "an empty value": ""
}
```

Serialized maps are inherently ordered, but the data structures they represent do not necessarily preserve order. Nice guarantees that the order of the map keys, as they were encountered in the document, is preserved. Serialized maps can also represent multiple entries that have the same key. This is not generally useful (if you need to have multiple values for a given key, its corresponding value should be a list) and cannot typically be represented by a map data structure. The Nice parser can be configured to produce a parse error when a duplicate key is encountered (the default behavior) or it can preserve either only first encountered duplicate value or only the last encountered duplicate value (in this case, the map order preserves the index of the last encountered duplicate, which may be less efficient if many duplicates exist, since it requires performing an ordered remove on the previously encountered instance).

ASCII spaces following the key scalar will be ignored, allowing adjacent values to be justified. The key scalar itself may not contain trailing or leading whitespace. A line only ever contains a single key scalar, unlike YAML. Maps must be nested using structural indentation.

```nice
fully aligned: value: 1
values:        value: 2
```

```JSON
{
    "fully aligned": "value: 1",
    "values": "value: 2"
}
```

### Inline Maps

The final syntactic construct is the inline map, which is, as its name hopefully suggests, the map equivalent of an inline list. An inline map is introduced by an opening curly brace `{` and closed by an opposing brace `}`. An inline map consists of a sequence of key-value pairs with the keys being separated from the values by the `:` character. An inline map may contain scalars, inline lists, and other inline maps as values, and all of its keys must be scalars. As with inline lists, whitespace surrounding values is ignored, and whitespace preceding keys is also ignored (there must be no whitespace between the key and its following `:`).

```nice
an example: { this: is, an inline: map }
nests:
    - { a list: [ of, { inline: maps } ] }
```

```JSON
{
    "an example": {"this": "is", "an inline": "map"},
    "nests": [
        { "a list": [ "of", { "inline": "maps" } ] }
    ]
}
```


## Restrictions

Nice documents must be encoded in valid UTF-8 with no BOM. They must use `LF`-only newlines (`CR` characters are forbidden). Tabs and spaces cannot be mixed for indentation. Indentation *must* adhere to a consistent quantum throughout the whole document, including on comment lines. Nonprinting ASCII characters are forbidden (specifically, any character less than `0x20` (space) except for `0x09` (horizontal tab) and `0x0A` (newline)). Trailing whitespace, including lines consisting only of whitespace, is forbidden, although empty lines are permitted. Some keys and values cannot be represented (for example, map keys cannot start with the character `#`, though map values can).

## Philosophy

### Let the Application Interpret Data Types (Bring Your Own Schema)

An arbitrarily structured data format with strict types adds complexity to the parser and cannot possibly cover all necessary types needed for every possible application. For example, numbers in JSON are represented by a sequence of ASCII characters, but they are defined by the format to be restricted to specifying double precision floating point numbers. Of course, it is possible to generate a numeric ASCII sequence that does not fit into a double precision floating point number. If an application needs to represent a 64-bit integer in JSON without producing technically invalid JSON, the value must be serialized as a string, which places the burden of decoding it on the application, since the format cannot represent it as a direct numeric value. The same is true of an RFC 3339 datetime. It's not possible for a format to account for every possible data type that an application may need, so don't bother. Users are encouraged to parse Nice documents directly into well-defined, typed structures. If you're interested, the NestedText documentation contains [several examples of why having strict data types in your serialization format is not as useful as you think][only-strings].

Nice explicitly differentiates between bare scalars and strings so that `null` may be disambiguated and interpreted differently from `"null"`.

### Fewer Rules over Flexibility

Nice is not, and does not try to be, a general-purpose data serialization format. There are, in fact, many values that simply cannot be represented Nicely. For example, map keys cannot start with a variety of characters, including `#`, `{`, `[`, or whitespace, which is a conscious design choice. In general, Nice is not a format designed with any emphasis placed on ease of programmatic production. While creating software that produces valid Nice data is certainly possible, this reference implementation has no functionality to do so.

### There's No Need to Conquer the World

Nice has no exhaustive specification or formal grammar. The parser is handwritten, and there are pretty much guaranteed to be some strange edge cases that weren't considered when writing it. Standardization is a good thing, generally speaking, but it's not a goal here. Perhaps this driven by the author's indolence more than deep philosophical zealotry. On the other hand, this paragraph is under the philosophy section.

# The Implementation

The Reference™ Nice parser/deserializer is this Zig library. It contains a handwritten nonrecursive parser to a generic data structure (`nice.Value`, a tagged union that can represent a scalar, a string, a list of these generic values, or a map of scalars to these generic values). The included example scripts demonstrate how to use the API. See `examples/parse.zig` for one-shot parsing from a slice. `examples/stream.zig` demonstrates how to parse streaming data that does not require loading a whole document into memory at once. This is slower but will generally have a lower peak memory usage (though that is mainly driven by the size of the document).

`nice.Value` has a method to recursively be converted into a strongly
typed user-defined structure. Zig's compile-time reflection is used to generate code to perform appropriate type conversion. There a variety of options which can be used to control specific details of the conversion, which are governed by `nice.parser.Options`. `examples/reify.zig` demonstrates basic use of this functionality.

A reference to a `nice.Diagnostics` object with a lifecycle at least as long as the parser must always be provided when parsing. If the source document could not be parsed, this diagnostic object will contain a human-readable explanation of the invalid syntax in the source document that caused the parser to error.

## Memory Strategy

The parser wraps a user-provided allocator in an arena, which is used for all internal allocations. All parsed values are copied into the arena rather than storing references to the source document. The parse result contains a reference to the arena, which can be used to free all of the data allocated during parsing.

# Disclaimer

It's entirely possible you hate this and think it's not, in fact, a nice data format. That's fine, but, unfortunately, you forgot to make a time machine and go back in time to make me name it something else. And yeah, this is probably impossible to search for.

# FAQ

Q: This is so similar to NestedText, why on earth didn't you just implement that?

A: in my opinion, it's extremely stupid that NestedText does not support indentation using tabs. Also, trailing whitespace is 100% satanic (in the bad way). And if an implementation is going to diverge there, it might as well roll in some other ideas, call it a new format, and just ruin the world with one more slightly-incompatible thing.

Q: Why is this documentation kind of bad?

A: I'll be honest, I ran out of steam while writing it. For a format that probably nobody besides me will ever use because there's so much open source code in the world that anything without heavy marketing tends to die in obscurity, it's a lot of work to write down the things I already know. But I have put an FAQ section here, while also indicating nobody has ever asked questions about this. Hmm.

# License

What are you going to do, steal my open-source code? Oh, noooooooooo. Here, let me help you.

Library is licensed MIT, examples are Public Domain/CC0. See file headers and the file `license` in the source tree for details.

[NestedText]: https://nestedtext.org
[only-strings]: https://nestedtext.org/en/latest/alternatives.html#only-strings
[YAML]: https://yaml.org
