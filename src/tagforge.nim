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

import tagforge/private/stew/endians2

type
  TagNodeKind* = enum
    ## NBT tags
    End, Byte, Short, Int, Long, Float, Double, ByteArray, String, List, Compound, IntArray, LongArray

  TagNodeObj* {.acyclic.} = object
    ## NBT node type
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
        name*: string
        compoundVal*: seq[TagNode]

      of IntArray:
        intArrVal*: seq[int32]

      of LongArray:
        longArrVal*: seq[int64]

  TagNode* = ref TagNodeObj ## NBT node type

func parse*(nbt: openArray[byte | char]): TagNode =
  ## Parses NBT data from a string/byte array
  discard