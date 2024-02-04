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
  streams,
  tables,
  json
]

import zippy
import mutf8

import tagforge/private/stew/endians2

type
  TagParsingError* = object of ValueError
  TagDataTooLarge* = object of TagParsingError

  TagValidationError* = object of ValueError

  TagFormat* = enum
    ## The fornat for parsing/dumping NBT
    BE, LE # BigEndian, LittleEndian
    # TODO: Implement VarInt encoding, seems a lot more tedious though...

  TagNodeKind* = enum
    ## NBT tags
    End = 0x00.byte
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

  TagNode* {.acyclic.} = ref object
    ## NBT node type
    # Acyclic since it'll never have a reference to itself, only children
    name: string

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
        compoundVal*: Table[string, TagNode]

      of IntArray:
        intArrVal*: seq[int32]

      of LongArray:
        longArrVal*: seq[int64]

func `[]`*(node: TagNode, name: string): TagNode =
  ## Access a named tag
  if node.kind != Compound:
    raise newException(TagValidationError, "Expected type `Compound` but got `" & $node.kind & "`!")

  node.compoundVal[name]

func `[]=`*(node: TagNode, name: string, value: TagNode) =
  ## Set a named tag
  if node.kind != Compound:
    raise newException(TagValidationError, "Expected type `Compound` but got `" & $node.kind & "`!")

  node.compoundVal[name] = value


func newTagByte*(x: int8): TagNode =
  ## Returns a byte TagNode with a signed byte as the value.
  TagNode(kind: Byte, byteVal: x)

func newTagShort*(x: int16): TagNode =
  ## Returns a short TagNode with a signed short as the value.
  TagNode(kind: Short, shortVal: x)

func newTagInt*(x: int32): TagNode =
  ## Returns an int TagNode with a signed int as the value.
  TagNode(kind: Int, intVal: x)

func newTagLong*(x: int64): TagNode =
  ## Returns a long TagNode with a signed long as the value.
  TagNode(kind: Long, longVal: x)

func newTagFloat*(x: float32): TagNode =
  ## Returns a float TagNode with a float as the value.
  TagNode(kind: Float, floatVal: x)

func newTagDouble*(x: float64): TagNode =
  ## Returns a double TagNode with a double as the value.
  TagNode(kind: Double, doubleVal: x)

func newTagByteArray*(x = newSeq[int8]()): TagNode =
  ## Returns a byte array TagNode with a seq[int8] as the value.
  TagNode(kind: ByteArray, byteArrVal: x)

func newTagString*(x = ""): TagNode =
  ## Returns a string TagNode with a string as the value.
  TagNode(kind: String, strVal: x)

func newTagList*(typ: TagNodeKind, x = newSeq[TagNode]()): TagNode {.raises: [TagValidationError].} =
  ## Returns a list TagNode with a seq[TagNode] as the value.
  for i in x:
    if i.kind != typ:
      raise newException(TagValidationError, "Expected type `" & $typ & "` but got `" & $i.kind & "`!")

  TagNode(kind: List, typ: typ, listVal: x)

func newTagCompound*(x = initTable[string, TagNode]()): TagNode =
  ## Returns a compound TagNode with a Table[string, TagNode] as the value.
  TagNode(kind: Compound, compoundVal: x)

func newTagIntArray*(x = newSeq[int32]()): TagNode =
  ## Returns an int array TagNode with a seq[int32] as the value.
  TagNode(kind: IntArray, intArrVal: x)

func newTagLongArray*(x = newSeq[int64]()): TagNode =
  ## Returns a long array TagNode with a seq[int64] as the value.
  TagNode(kind: LongArray, longArrVal: x)

# Format-specific reading
proc read[T: SomeNumber](s: Stream, format: TagFormat): T =
  ## Read a number from a string using a specific format
  const size = sizeof T

  template fromBytesFormat[T: SomeEndianInt](typ: typedesc[T]): T =
    if format == BE:
      fromBytesBE(typ, cast[seq[byte]](s.readStr(size)))
    else:
      fromBytesLE(typ, cast[seq[byte]](s.readStr(size)))

  when size == 1:
    result = cast[T](fromBytesFormat(uint8))

  elif size == 2:
    result = cast[T](fromBytesFormat(uint16))

  elif size == 4:
    result = cast[T](fromBytesFormat(uint32))

  elif size == 8:
    result = cast[T](fromBytesFormat(uint64))

  else:
    {.error: "Unsupported type.".}

proc readString(s: Stream, format: TagFormat): string =
  let length = s.read[:uint16](format).int
  return decodeMutf8(cast[seq[byte]](s.readStr(length)))

# This code is taken from https://github.com/Yardanico/nimnbt/blob/master/src/nimnbt.nim#L60-L120
# which was licensed under MIT
proc parseNbtInternal(s: Stream, format: TagFormat, tagKind = End, parseName: static bool = true): TagNode =
  try:
    result = TagNode(kind: if tagKind == End: TagNodeKind(s.readUint8()) else: tagKind)
  except IOError:
    return TagNode(kind: End)

  if result.kind == End: return

  if parseName: result.name = s.readString(format)
  case result.kind
    of End: return
    of Byte:
      result.byteVal = s.readInt8()
    of Short:
      result.shortVal = s.read[:int16](format)
    of Int:
      result.intVal = s.read[:int32](format)
    of Long:
      result.longVal = s.read[:int64](format)
    of Float:
      result.floatVal = s.read[:float32](format)
    of Double:
      result.doubleVal = s.read[:float64](format)
    of ByteArray:
      let size = s.read[:int32](format)
      var i = 0
      result.byteArrVal = newSeqOfCap[int8](size)

      while i < size:
        result.byteArrVal.add(s.readInt8())
        inc i
    of String:
      result.strVal = s.readString(format)
    of List:
      result.typ = s.read[:uint8](format).TagNodeKind
      let size = s.read[:uint32](format).int
      var i = 0
      result.listVal = newSeqOfCap[TagNode](size)

      while i < size:
        # Tags in lists don't have names at all
        result.listVal.add(s.parseNbtInternal(format, result.typ, parseName = false))
        inc i
    of Compound:
      result.compoundVal = initTable[string, TagNode]()
      while true:
        var nextTag = s.parseNbtInternal(format)
        if nextTag.kind == End: break
        result.compoundVal[nextTag.name] = nextTag
    of IntArray:
      let size = s.read[:int32](format).int
      var i = 0
      result.intArrVal = newSeqOfCap[int32](size)

      while i < size:
        result.intArrVal.add s.read[:int32](format)
        inc(i)
    of LongArray:
      let size = s.read[:int32](format).int
      var i = 0
      result.longArrVal = newSeqOfCap[int64](size)

      while i < size:
        result.longArrVal.add s.read[:int64](format)
        inc(i)

proc parseNbt*(s: string, format = BE, network = false): TagNode =
  ## Parses NBT data structure from the string *s*
  ## 
  ## *format* specifies the endianness of the data
  ## *network* specifies whether the data is sent over the network
  result = newTagCompound()

  var strm: Stream
  try:
    strm = newStringStream(s.uncompress)
  except ZippyError:
    strm = newStringStream(s)

  if network:
    result[""] = strm.parseNbtInternal(format, parseName = false)
  else:
    while true:
      var nextTag = strm.parseNbtInternal(format)
      if nextTag.kind == End: break
      result.compoundVal[nextTag.name] = nextTag

proc toJson*(s: TagNode): JsonNode =
  ## Converts an NBT tag to the JSON for printing/serialization
  case s.kind
    of End:
      return

    of Byte:
      result = %s.byteVal
    of Short:
      result = %s.shortVal
    of Int:
      result = %s.intVal
    of Long:
      result = %s.longVal
    of Float:
      result = %s.floatVal
    of Double:
      result = %s.doubleVal
    of ByteArray:
      result = %s.byteArrVal
    of String:
      result = %s.strVal

    of List:
      result = newJArray()
      for i in s.listVal:
        result.add toJson(i)

    of Compound:
      result = newJObject()
      for k, v in s.compoundVal:
        result[k] = toJson(v)

    of IntArray:
      result = %s.intArrVal
    of LongArray:
      result = %s.longArrVal

proc `$`*(t: TagNode): string =
  ## Converts Tag to a string for easier debugging/visualising
  $toJson(t)

func `==`*(a, b: TagNode): bool =
  ## Compares two tags
  if a.kind != b.kind: return false

  case a.kind
    of End: return true
    of Byte: return a.byteVal == b.byteVal
    of Short: return a.shortVal == b.shortVal
    of Int: return a.intVal == b.intVal
    of Long: return a.longVal == b.longVal
    of Float: return a.floatVal == b.floatVal
    of Double: return a.doubleVal == b.doubleVal
    of ByteArray: return a.byteArrVal == b.byteArrVal
    of String: return a.strVal == b.strVal
    of List: return a.listVal == b.listVal
    of Compound: return a.compoundVal == b.compoundVal
    of IntArray: return a.intArrVal == b.intArrVal
    of LongArray: return a.longArrVal == b.longArrVal