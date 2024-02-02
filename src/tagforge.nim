#! Copyright 2024 Yu-Vitaqua-fer-Chronos
#!
#! Licensed under the Apache License, Version 2.0 (the "License");
#! you may not use this file except in compliance with the License.
#! You may obtain a copy of the License at
#!
#!     http://www.apache.org/licenses/LICENSE-2.0
#!
#! Unless required by applicable law or agreed to in writing, software
#! distributed under the License is distributed on an "AS IS" BASIS,
#! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#! See the License for the specific language governing permissions and
#! limitations under the License.
import std/[
  strformat,
  streams,
  options
]

import tagforge/private/stew/endians2

type
  TagParsingError* = object of ValueError
  TagDataTooLarge* = object of TagParsingError

  TagValidationError* = object of ValueError

  TagFormat* = enum
    ## The fornat for parsing/dumping NBT
    BE, LE, VI # BigEndian, LittleEndian, VarInt

  TagNodeKind* = enum
    ## NBT tags
    End = 0x00'b
    Byte = 0x01
    Short = 0x02
    Int = 0x03
    Long = 0x04
    Float = 0x05
    Double = 0x06
    ByteArray = 0x07
    String = 0x08
    List = 0x09
    Compound = 0x0A
    IntArray = 0x0B
    LongArray = 0x0C

  NamedTag* = object
    name*: string
    node*: TagNode

  TagNodeObj* {.acyclic.} = object
    ## NBT node type
    # Acyclic since it'll never have a reference to itself, only children
    case kind*: TagNodeKind
      of End:
        discard

      of Byte:
        byteVal*: int8

      of Short:
        shortVal*: int16

      of Int:
        intVal*: int32

      of Long:
        longVal*: int64

      of Float:
        floatVal*: float32

      of Double:
        doubleVal*: float64

      of ByteArray:
        byteArrVal*: seq[int8]

      of String:
        strVal*: string

      of List:
        typ*: TagNodeKind
        listVal*: seq[TagNode]

      of Compound:
        name*: Option[string]
        compoundVal*: seq[NamedTag]

      of IntArray:
        intArrVal*: seq[int32]

      of LongArray:
        longArrVal*: seq[int64]

  TagNode* = ref TagNodeObj ## NBT node type

func newTagCompound*(name: string, compoundVal = newSeq[NamedTag]()): TagNode =
  ## Create a new named compound tag.
  TagNode(kind: Compound, name: name, compoundVal: compoundVal)

func newTagCompound*(compoundVal = newSeq[NamedTag]()): TagNode =
  ## Create a new unnamed compound tag (an empty string is used as a placeholder).
  TagNode(kind: Compound, name: "", compoundVal: compoundVal)

func newTagList*(typ: TagNodeKind, listVal = newSeq[TagNode]()): TagNode {.raises: [TagValidationError].} =
  ## Create a new list tag with a type!
  result = TagNode(kind: List, typ: typ, listVal: listVal)

  when not (defined(danger) or defined(tagForgeNoValidation)):
    # Disable with d:danger
    for i in listVal:
      if i.kind != typ:
        raise newException(TagValidationError, &"Index {i} expected type `{typ}` but got `{i.kind}`!")

template readNodeKind(data: Stream): TagNodeKind =
  try:
    TagNodeKind(data.readUint8().byte)
  except RangeDefect:
    raise newException(TagParsingError, fmt"Invalid node type at position {data.getPosition() - 1}")

proc readSignedInt(data: Stream): int32 =
  when cpuEndian != BigEndian:
    let listSize = cast[int32](data.readUint32().toLE())
  else:
    let listSize = data.readInt32()

proc parse*(data: Stream, format=BE, nodeLimit=1024,
  networkFormat: bool = false): TagNode =
  ## Parses NBT data from a stream.
  ## `format` defaults to BigEndian decoding.
  ## `networkFormat` is specific to Java 1.20.2 and onwards.
  var
    depth = 0
    nodesParsed = 0
    root = newTagCompound()
    parent = @[root]

  while not data.atEnd:
    if nodesParsed >= nodeLimit:
      raise newException(TagDataTooLarge, "")

    let
      typ = data.readNodeKind()
      parentCompound = parent[^1].kind == Compound

    # Handle outer compound type
    if parentCompound:
      # If compound type
      if typ == Compound:
        parent[^1].compoundVal.add newTagCompound()
        inc nodesParsed
        # If not network format, read name
        if not (networkFormat and (depth == 0)):
          parent[^1].compoundVal[^1].name = data.readStr(data.readSignedInt())

        # Add as parent
        parent.add parent[^1].compoundVal[^1].node
        inc depth
        continue

      if typ == List:
        # The type + size of the list
        let
          listTyp = data.readNodeKind()
          listSize = data.readSignedInt()

        # The list itself
        var list = newSeq[TagNode](listSize)

        parent[^1].compoundVal.add newTagList(listTyp, list)
        inc nodesParsed
        parent.add parent[^1].compoundVal[^1].node
        inc depth
        continue
