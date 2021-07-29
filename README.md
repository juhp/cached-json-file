# cached-json-file

A Haskell library providing a cached json file.

Useful for frequently using some remote json data which changes slowly.

## Usage

`getCachedJSON dir file url minutes` caches the json obtained from url in
`~/.cache/dir/file` for `minutes`.

eg:

```haskell
import System.Cached.JSON

getSnapshots :: IO Object
getSnapshots =
  getCachedJSON "stackage-snapshots" "snapshots.json" "http://haddock.stackage.org/snapshots.json" 200
```

If you call getSnapshots several times within 200 minutes it will be read
from the local cache rather than downloading the same data multiple times.
This also helps to speed up the program.

There is also `getCachedJSONQuery prog jsonfile webquery minutes`
which takes a `webquery :: (FromJSON a, ToJSON a) => IO a` to download
the json data.
