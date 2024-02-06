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
  math,
  os
]

import tagforge

const NbtDir = currentSourcePath() / ".." / "nbt"

suite "NBT Parsing":
  test "Hello World - Parsing":
    let data = readFile(NbtDir / "hello_world.nbt").parseNbt(BE)
    let myData = newTagCompound({"hello world": newTagCompound({"name": newTagString("Bananrama")}.toTable)}.toTable)

    #echo myData == myData

    check data == myData

  test "Nested List - Parsing":
    let data = readFile(NbtDir / "nested_list.nbt").parseNbt(BE)
    let myData = newTagCompound({"hello world": newTagCompound({
        "A": newTagList(List, @[newTagList(Int, @[newTagInt(123)])])
      }.toTable)
    }.toTable)

    check data == myData

  test "Big Test - Parsing":
    let data = readFile(NbtDir / "bigtest.nbt").parseNbt(BE)

    check true # Just need to make sure it doesn't throw an exception
  
  test "Nan Value - Parsing":
    let data = readFile(NbtDir / "nan-value.dat").parseNbt(BE)

    check data[""]["Pos"][1].getDouble().isNan

suite "NBT Dumping":
  test "Hello World - Dumping":
    let data = newTagCompound({"hello world": newTagCompound({"name": newTagString("Bananrama")}.toTable)}.toTable)

    check data.dumpNbt().parseNbt() == data

  test "Nested List - Dumping":
    let data = newTagCompound({"hello world": newTagCompound({
        "A": newTagList(List, @[newTagList(Int, @[newTagInt(123)])])
      }.toTable)
    }.toTable)

    writeFile("test.nbt", data.dumpNbt())

    check data.dumpNbt().parseNbt() == data