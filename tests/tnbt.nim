# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import std/[
  unittest,
  tables,
  json,
  os
]

import tagforge

const NbtDir = currentSourcePath() / ".." / "nbt"

suite "NBT Parsing":
  test "hello_world.nbt":
    let data = readFile(NbtDir / "hello_world.nbt").parseNbt(BE)
    let myData = newTagCompound({"hello world": newTagCompound({"name": newTagString("Bananrama")}.toTable)}.toTable)

    #echo myData == myData

    check data == myData

  test "bigtest.nbt":
    let data = readFile(NbtDir / "bigtest.nbt").parseNbt(BE)

    check true # Just need to make sure it doesn't throw an exception