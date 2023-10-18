Have you ever wished someone would walk up to you and say, in a tremendously exaggerated, stereotypical surfer voice, "nice data, dude"? Well, wish no longer because now your data can be nice by definition, due to our patented Manipulative Marketing Naming technique. Introducing!

# Nice Data: There's no Escape

```nice
# this is an example of some nice data.
project:
    name: nice data
    description:
        | A file format for storing structured data. Nice uses syntactic whitespace
        + to represent the data structure. It defines two types of data, scalars and
        + strings, which are be used to compose its two data structures, lists and maps.
        >
        > Nice to write, nice to read.
    inspiration:
        - { name: NestedText, url: https://nestedtext.org }
        - { name: YAML,       url: https://yaml.org }
        - A fervent dislike of TOML
    non-goals: [ general-purpose data serialization, world domination ]
    epic freaking funny number lol: 42069580089001421337666
```

Nice Data is a format for storing structured data in a file. It's pleasant to read and adheres to the philosophy that form should match structure. It's heavily inspired by [NestedText], though it also looks similar to [YAML].

## Syntax

- structured indentation using tabs or spaces
- scalars
- strings
  - line, space, and concat strings
- lists
- inline lists
- maps
- inline maps


## Restrictions

Nice documents must be encoded in valid UTF-8. They must use `LF`-only newlines (`CR` characters are forbidden). Tabs and spaces cannot be mixed for indentation. Indentation *must* adhere to a consistent quantum. Nonprinting ASCII characters are forbidden. Trailing whitespace, including lines consisting only of whitespace, is forbidden, although empty lines are permitted. Some keys and values cannot be represented.

## Philosophy

### Let the Application Interpret Data Types (Bring Your Own Schema)

An arbitrarily structured data format with strict types makes the format more work to parse and is a challenge. Numbers in JSON are represented by a sequence of ASCII characters, but they are defined by the format to represent double precision floating point numbers. Of course, it is possible to generate a numeric ASCII sequence that does not fit into a double precision floating point number. If you want to represent a 128-bit integer in JSON, you have to encode it as a string, and decode it in your application, as the format cannot accommodate it as a direct numeric value. The same is true of an RFC 3339 datetime. It's not possible for a format to account for every possible data type that an application may need, so don't bother. Users are encouraged to parse Nice documents directly into well-defined, typed structures.

Nice explicitly differentiates between bare scalars and strings so that `null` may be disambiguated and interpreted differently from `| null`.

### Simplicity over Flexibility. What You See is What You Get

Nice is not, and does not try to be, a general-purpose data serialization format. There are, in fact, many values that simply cannot be represented in nice. For example, map keys cannot start with the a variety of characters, including `#`, `-`, `>`, `+`, `|`, or whitespace, and this is a conscious design choice. In general, nice is not a format designed with consideration for being produced by a computer. While programmatic serialization is certainly possible, this reference implementation has no functionality to do so.

### There's No Need to Conquer the World

Nice has no exhaustive specification or formal grammar. The parser is handwritten, and there are pretty much guaranteed to be some strange edge cases that weren't considered when writing it. Standardization is a good thing, generally speaking, but it's not a goal here.

# The Implementation

[NestedText]: https://nestedtext.org
[YAML]: https://yaml.org
