# canteven-config


Turns out, all of our executables shared the same sort of pattern: Parse
the command line to locate a yaml config file, read and parse the config
file, set up `hslogger` based on some stuff found in the config file,
and pass the rest of the config to the actual program. This library makes
doing that pattern super easy. It is meant to be a rather particular
(as opposed to general) solution for this sort of problem.

## Usage:

```haskell
import Canteven.Config (canteven)
import Data.Aeson (FromJSON(parseJSON))

data MyConfig = MyConfig {
    someValue :: Int,
    someUrl :: String
  }
instance FromJSON MyConfig where
  parseJSON = # ...

main = do
  config <- canteven :: IO MyConfig
  # ...
```

## YAML

Even though you see `FromJSON`.. that really means yaml.. see the `yaml`
package for more info.
