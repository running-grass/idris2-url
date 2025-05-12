# idris2-url
WIP……  

A URL library for idris2, Based on [RFC 1738](https://www.rfc-wiki.org/wiki/RFC1738).

## Feature
- [x] General Sytax
- [x] HTTP URL parse and stringify
- [ ] HTTP URL utils
- [ ] FTP URL

## Test

```bash
pack test url
```

Or

```bash
find . -name '*.idr' | entr -s 'pack test url'
```