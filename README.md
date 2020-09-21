# SimParser

![CI](https://github.com/giuuliorusso/simparser/workflows/CI/badge.svg)

Simple Haskell parsing library.

## Usage

```haskell
import SimParser

-- GHCI Examples

> parse digit "012345"
Right ('0',"12345")

> parse (char 'x') "abc"
Left (ParserError "Parsing failed: expected 'x', found 'a'")

> parse (some digit) "012345abc"
Right ("012345","abc")

> parse (some (digit <|> letter)) "012345abc"
Right ("012345abc","")

> parse int "-10n"
Right (-10,"n")
```

## Acknowledgements

SimParser is based on Graham Hutton's [Functional parsing library](https://www.cs.nott.ac.uk/~pszgmh/Parsing.hs) [[YouTube](https://youtu.be/dDtZLm7HIJs)].

## License

[MIT](./LICENSE)
