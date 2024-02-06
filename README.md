# TagForge
An NBT library written in Nim to parse NBT data, using https://wiki.vg/NBT to help me implement it!

## Usage
Probably better off looking at the docs and tests, but here:
```nim
let data = readFile("my.nbt").parseNbt()

echo data["rootTag"]["name"].getString()
data["rootTag"]["name"] = newTagString("new name")

writeFile("my.modified.nbt")
```

## To-Do
- [ ] Write `to` and `from` macros for types.
  - I'd say the main issue with this is the fact that it'd be a lossy conversion (since the type of list may be lost), but it shouldn't be an *actual* issue.
- [ ] More comprehensive tests including Bedrock NBT.

## Credits
Thanks to Yardanico for making [NimNBT](https://github.com/Yardanico/nimnbt) which was a *massive* help for this project, since this was my first time making any sort of parser!

Thanks to wiki.vg for the [specification](https://wiki.vg/NBT#Specification) which gave examples and up-to-date info about the formats!