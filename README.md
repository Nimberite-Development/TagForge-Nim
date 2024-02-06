# TagForge
An NBT library written in Nim to parse NBT data, using https://wiki.vg/NBT to help me implement it!

## To-Do
- [ ] Write `to` and `from` macros for types.
  - I'd say the main issue with this is the fact that it'd be a lossy conversion (since the type of list may be lost), but it shouldn't be an *actual* issue.
- [ ] More comprehensive tests including Bedrock NBT.