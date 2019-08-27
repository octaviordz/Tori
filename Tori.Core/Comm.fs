module Comm

open System.Text
open System
open System.Diagnostics
open System.Diagnostics.CodeAnalysis

type LineKind =
    | Tversion = 100
    | Rversion = 101
    | Tauth = 102
    | Rauth = 103
    | Terror = 106 // ilegal
    | Rerror = 107
    | Tread = 116
    | Rread = 117
    | Twrite = 118
    | Rwrite = 119
    | Tmax = 120

/// Byte size for a character encoded in UTF8
[<SuppressMessage("*", "ActivePatternNames")>]
[<Measure>]
type b8

type KindTagNote = LineKind * uint16 * string

[<Struct>]
type Line =
    { size : int<b8>
      kind : LineKind
      tag : uint16
      noteSize : int<b8>
      note : string }
    static member Default =
        { size = 0 * 1<b8>
          kind = LineKind.Tmax
          tag = 0us
          noteSize = 0 * 1<b8>
          note = String.Empty }

let serialize (kindTagNote : KindTagNote) (buffer : byte []) =
    let kind, tag, note = kindTagNote
    // TODO: Check BitConverter.IsLittleEndian
    // size[4] type[1] tag[2] nsize[2] note
    let nsize = Encoding.UTF8.GetByteCount(note)
    let size = 9 + nsize
    let nbytes = Encoding.UTF8.GetBytes(note)
    Buffer.BlockCopy(BitConverter.GetBytes((uint32) size), 0, buffer, 0, 4)
    buffer.[4] <- (byte) kind
    Buffer.BlockCopy(BitConverter.GetBytes(tag), 0, buffer, 5, 2)
    Buffer.BlockCopy(BitConverter.GetBytes(uint16 (nsize)), 0, buffer, 7, 2)
    Buffer.BlockCopy(nbytes, 0, buffer, 9, nbytes.Length)
    { size = size * 1<b8>
      kind = kind
      tag = tag
      noteSize = nsize * 1<b8>
      note = note }

let deserialize (buffer : byte []) (size : int32) =
    // size[4] type[1] tag[2] nsize[2] note
    Debug.Assert((buffer.Length >= size))
    let kind = (int) buffer.[4]
    let tag = (uint16) (BitConverter.ToInt16(buffer, 5))
    let nsize = (int32) (BitConverter.ToInt16(buffer, 7))
    let note = Encoding.UTF8.GetString(buffer, 9, nsize)
    let kind : LineKind = enum kind
    { size = size * 1<b8>
      kind = kind
      tag = tag
      noteSize = nsize * 1<b8>
      note = note }
