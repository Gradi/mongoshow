#  mongoshow

mongoshow is a command line utility that scans MongoDB's collections
and generates HTML file with document schemes.

## Installation

```
cabal configure
cabal install
```

P.s. mongoshow isn't available on Hackage.

## Usage

Run mongoshow with *--help* flag to see instructions.

```
Usage: mongoshow [-h|--host ARG] [-o|--out ARG] [--db ARG] [-c|--collection ARG]
                 [--limit ARG]
  This program scans documents in MongoDB's collections and generates html page
  with schemes.

Available options:
  -h,--host ARG            MongoDB host (default: "localhost:27017")
  -o,--out ARG             Out file (default: "schemes.html")
  --db ARG                 Databases to scan. You may specify this option
                           several times. If not set then all dbs will be
                           scanned.
  -c,--collection ARG      Collections to scan. You may specify this option
                           several times. If not set then all collections will
                           be scanned.
  --limit ARG              Limits number of bson documents to read. If not set
                           then all documents will be scanned. (default: 0)
  -h,--help                Show this help text
```
